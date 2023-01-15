#include "LinearScan.h"
#include <algorithm>
#include <iostream>
#include "LiveVariableAnalysis.h"
#include "MachineCode.h"

LinearScan::LinearScan(MachineUnit* unit) {
    this->unit = unit;
    // 这里不对r0-r3做分配嘛
    for (int i = 4; i < 11; i++)
        regs.push_back(i);
}

void LinearScan::allocateRegisters() {
    for (auto& f : unit->getFuncs()) {
        func = f;
        bool success;
        success = false;
        while (!success)  // repeat until all vregs can be mapped
        {
            computeLiveIntervals();
            success = linearScanRegisterAllocation();
            if (success)  // all vregs can be mapped to real regs
                modifyCode();
            else  // spill vregs that can't be mapped to real regs
                genSpillCode();
        }
    }
}

void LinearScan::makeDuChains() {
    LiveVariableAnalysis lva;
    lva.pass(func);
    du_chains.clear();
    int i = 0;
    std::map<MachineOperand, std::set<MachineOperand*>> liveVar;
    for (auto& bb : func->getBlocks()) {
        liveVar.clear();
        for (auto& t : bb->getLiveOut())
            liveVar[*t].insert(t);
        int no;
        no = i = bb->getInsts().size() + i;
        for (auto inst = bb->getInsts().rbegin(); inst != bb->getInsts().rend();
             inst++) {
            (*inst)->setNo(no--);
            for (auto& def : (*inst)->getDef()) {
                if (def->isVReg()) {
                    auto& uses = liveVar[*def];
                    du_chains[def].insert(uses.begin(), uses.end());
                    auto& kill = lva.getAllUses()[*def];
                    std::set<MachineOperand*> res;
                    set_difference(uses.begin(), uses.end(), kill.begin(),
                                   kill.end(), inserter(res, res.end()));
                    liveVar[*def] = res;
                }
            }
            for (auto& use : (*inst)->getUse()) {
                if (use->isVReg())
                    liveVar[*use].insert(use);
            }
        }
    }
}

void LinearScan::computeLiveIntervals() {
    makeDuChains();
    intervals.clear();
    for (auto& du_chain : du_chains) {
        int t = -1;
        for (auto& use : du_chain.second)
            t = std::max(t, use->getParent()->getNo());
        Interval* interval = new Interval({du_chain.first->getParent()->getNo(),
                                           t,
                                           false,
                                           0,
                                           0,
                                           {du_chain.first},
                                           du_chain.second});
        intervals.push_back(interval);
    }
    bool change;
    change = true;
    while (change) {
        change = false;
        std::vector<Interval*> t(intervals.begin(), intervals.end());
        for (size_t i = 0; i < t.size(); i++)
            for (size_t j = i + 1; j < t.size(); j++) {
                Interval* w1 = t[i];
                Interval* w2 = t[j];
                if (**w1->defs.begin() == **w2->defs.begin()) {
                    std::set<MachineOperand*> temp;
                    set_intersection(w1->uses.begin(), w1->uses.end(),
                                     w2->uses.begin(), w2->uses.end(),
                                     inserter(temp, temp.end()));
                    if (!temp.empty()) {
                        change = true;
                        w1->defs.insert(w2->defs.begin(), w2->defs.end());
                        w1->uses.insert(w2->uses.begin(), w2->uses.end());
                        w1->start = std::min(w1->start, w2->start);
                        w1->end = std::max(w1->end, w2->end);
                        auto it =
                            std::find(intervals.begin(), intervals.end(), w2);
                        if (it != intervals.end())
                            intervals.erase(it);
                    }
                }
            }
    }
    sort(intervals.begin(), intervals.end(), compareStart);
}

bool LinearScan::linearScanRegisterAllocation() {
    bool success = true;
    active.clear();
    regs.clear();
    for (int i = 4; i < 11; i++)
        regs.push_back(i);
    for (auto& i : intervals) {
        expireOldIntervals(i);
        if (regs.empty()) {
            spillAtInterval(i);
            // 不知道是不是该这样
            success = false;
        } else {
            i->rreg = regs.front();
            regs.erase(regs.begin());
            active.push_back(i);
            sort(active.begin(), active.end(), compareEnd);
        }
    }
    return success;
}

void LinearScan::modifyCode() {
    for (auto& interval : intervals) {
        func->addSavedRegs(interval->rreg);
        for (auto def : interval->defs)
            def->setReg(interval->rreg);
        for (auto use : interval->uses)
            use->setReg(interval->rreg);
    }
}

//这个就是溢出代码的处理和生成，因为在寄存器分配中免不了处理这个
void LinearScan::genSpillCode() 
{   
    for (auto& interval : intervals)//遍历所有区间
     {  

        //首先预定一个判决因子
        auto judgement=(!interval->spill);

        if (judgement)//不是活跃区间
            continue;
        // TODO
        /* HINT:
         * The vreg should be spilled to memory.
         * 1. insert ldr inst before the use of vreg
         * 2. insert str inst after the def of vreg
         */

         //上面所给的提示其实是非常易懂的
         /*
         遍历use指令列表，在use指令前插入loadminstruction，将之从栈内加载到目前的虚拟寄存器中
         遍历def指令的列表，在def指令后插入storeminstruction，将之从目前的虚拟寄存器中存到栈内
         */

        //因为你的栈要往上抬，其实是加上一个负数的
        //disp其实是为了取偏移量
        //再去给off
        interval->disp = -func->AllocSpace(4);
        auto off = new MachineOperand(MachineOperand::IMM, interval->disp);//偏移量
        auto fp = new MachineOperand(MachineOperand::REG, 11);//栈指针，11号寄存器

        /*
        for (auto use : interval->uses) 这个循环用来遍历每一个需要溢出的活跃区间的所
        有使用虚拟寄存器的指令。在这个循环中，每一个指令都会插入一个加载指令（ldr），用来从内存中加载虚拟寄存器的值。
        */
        for (auto use : interval->uses) 
        {
            auto temp = new MachineOperand(*use);
            MachineOperand* operand = nullptr;


            //对特殊情况进行处理：
            auto part3=(interval->disp > 255);
            auto part4=(interval->disp < -255);


            //预定义一个判决因子
            auto judgement2=(part3 || part4);

            if (judgement2) 
            {
                operand = new MachineOperand(MachineOperand::VREG,SymbolTable::getLabel());
                auto inst1 = new LoadMInstruction(use->getParent()->getParent(),operand, off);
                use->getParent()->insertBefore(inst1);
            }
            if (operand) 
            {
                auto inst = new LoadMInstruction(use->getParent()->getParent(), temp, fp, new MachineOperand(*operand));
                use->getParent()->insertBefore(inst);
            } 
            else 
            {
                auto inst = new LoadMInstruction(use->getParent()->getParent(),temp, fp, off);
                use->getParent()->insertBefore(inst);
            }
        }
        //再去看def列表
        /*
        * for (auto def : interval->defs) 这个循环用来遍历每一个需要溢出的活跃区间的所
        有定义虚拟寄存器的指令。在这个循环中，每一个指令都会插入一个存储指令（str），用来将虚拟寄存器的值存储到内存中。
        */
        for (auto def : interval->defs)
        {    

            //临时的
            auto temp = new MachineOperand(*def);
            //进行一些个初始化工作
            MachineOperand* operand = nullptr;
            MachineInstruction *inst = nullptr;
            MachineInstruction *inst1 = nullptr;
            
            //对特殊的情况进行处理
            auto part1=(interval->disp > 255);
            auto part2=(interval->disp < -255);


            //预定义一个判决因子
            auto judgement3=(part1 || part2);


            if (judgement3)
             {
                operand = new MachineOperand(MachineOperand::VREG,SymbolTable::getLabel());
                inst1 = new LoadMInstruction(def->getParent()->getParent(),operand, off);
                def->getParent()->insertAfter(inst1);
            }

            if (operand)
                inst =new StoreMInstruction(def->getParent()->getParent(), temp,fp, new MachineOperand(*operand));
            else
                inst = new StoreMInstruction(def->getParent()->getParent(),temp, fp, off);
                
            if (inst1)
                inst1->insertAfter(inst);
            else
                def->getParent()->insertAfter(inst);
        }
    }
}

void LinearScan::expireOldIntervals(Interval* interval) {
    auto it = active.begin();
    while (it != active.end()) {
        if ((*it)->end >= interval->start)
            return;
        regs.push_back((*it)->rreg);
        it = active.erase(find(active.begin(), active.end(), *it));
        sort(regs.begin(), regs.end());
    }
}

void LinearScan::spillAtInterval(Interval* interval) {
    auto spill = active.back();
    if (spill->end > interval->end) {
        spill->spill = true;
        interval->rreg = spill->rreg;
        active.push_back(interval);
        sort(active.begin(), active.end(), compareEnd);
    } else {
        interval->spill = true;
    }
}

bool LinearScan::compareStart(Interval* a, Interval* b) {
    return a->start < b->start;
}

bool LinearScan::compareEnd(Interval* a, Interval* b) {
    return a->end < b->end;
}