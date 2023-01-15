#include "Instruction.h"
#include <iostream>
#include <sstream>
#include "BasicBlock.h"
#include "Function.h"
#include "Type.h"
extern FILE* yyout;

Instruction::Instruction(unsigned instType, BasicBlock* insert_bb) {
    prev = next = this;
    opcode = -1;
    this->instType = instType;
    if (insert_bb != nullptr) {
        insert_bb->insertBack(this);
        parent = insert_bb;
    }
}

Instruction::~Instruction() {
    parent->remove(this);
}

BasicBlock* Instruction::getParent() {
    return parent;
}

void Instruction::setParent(BasicBlock* bb) {
    parent = bb;
}

void Instruction::setNext(Instruction* inst) {
    next = inst;
}

void Instruction::setPrev(Instruction* inst) {
    prev = inst;
}

Instruction* Instruction::getNext() {
    return next;
}

Instruction* Instruction::getPrev() {
    return prev;
}

BinaryInstruction::BinaryInstruction(unsigned opcode,
                                     Operand* dst,
                                     Operand* src1,
                                     Operand* src2,
                                     BasicBlock* insert_bb)
    : Instruction(BINARY, insert_bb) {
    this->opcode = opcode;
    operands.push_back(dst);
    operands.push_back(src1);
    operands.push_back(src2);
    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
}
void BinaryInstruction::replaceDef(Operand* new_) {
    operands[0]->removeDef(this);
    operands[0] = new_;
    new_->setDef(this);
}
BinaryInstruction::~BinaryInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

void BinaryInstruction::output() const {
    std::string s1, s2, s3, op, type;
    s1 = operands[0]->toStr();
    s2 = operands[1]->toStr();
    s3 = operands[2]->toStr();
    type = operands[0]->getType()->toStr();
    switch (opcode) {
        case ADD:
            op = "add";
            break;
        case SUB:
            op = "sub";
            break;
        case MUL:
            op = "mul";
            break;
        case DIV:
            op = "sdiv";
            break;
        case MOD:
            op = "srem";
            break;
        default:
            break;
    }
    fprintf(yyout, "  %s = %s %s %s, %s\n", s1.c_str(), op.c_str(),
            type.c_str(), s2.c_str(), s3.c_str());
}

CmpInstruction::CmpInstruction(unsigned opcode,
                               Operand* dst,
                               Operand* src1,
                               Operand* src2,
                               BasicBlock* insert_bb)
    : Instruction(CMP, insert_bb) {
    this->opcode = opcode;
    operands.push_back(dst);
    operands.push_back(src1);
    operands.push_back(src2);
    dst->setDef(this);
    src1->addUse(this);
    src2->addUse(this);
}
void CmpInstruction::replaceDef(Operand* new_) {
    operands[0]->removeDef(this);
    operands[0] = new_;
    new_->setDef(this);
}
CmpInstruction::~CmpInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
    operands[2]->removeUse(this);
}

void CmpInstruction::output() const {
    std::string s1, s2, s3, op, type;
    s1 = operands[0]->toStr();
    s2 = operands[1]->toStr();
    s3 = operands[2]->toStr();
    type = operands[1]->getType()->toStr();
    switch (opcode) {
        case E:
            op = "eq";
            break;
        case NE:
            op = "ne";
            break;
        case L:
            op = "slt";
            break;
        case LE:
            op = "sle";
            break;
        case G:
            op = "sgt";
            break;
        case GE:
            op = "sge";
            break;
        default:
            op = "";
            break;
    }

    fprintf(yyout, "  %s = icmp %s %s %s, %s\n", s1.c_str(), op.c_str(),
            type.c_str(), s2.c_str(), s3.c_str());
}

UncondBrInstruction::UncondBrInstruction(BasicBlock* to, BasicBlock* insert_bb)
    : Instruction(UNCOND, insert_bb) {
    branch = to;
}

void UncondBrInstruction::output() const {
    fprintf(yyout, "  br label %%B%d\n", branch->getNo());
}

void UncondBrInstruction::setBranch(BasicBlock* bb) {
    branch = bb;
}

BasicBlock* UncondBrInstruction::getBranch() {
    return branch;
}

CondBrInstruction::CondBrInstruction(BasicBlock* true_branch,
                                     BasicBlock* false_branch,
                                     Operand* cond,
                                     BasicBlock* insert_bb)
    : Instruction(COND, insert_bb) {
    this->true_branch = true_branch;
    this->false_branch = false_branch;
    cond->addUse(this);
    operands.push_back(cond);
}

CondBrInstruction::~CondBrInstruction() {
    operands[0]->removeUse(this);
}

void CondBrInstruction::output() const {
    std::string cond, type;
    cond = operands[0]->toStr();
    type = operands[0]->getType()->toStr();
    int true_label = true_branch->getNo();
    int false_label = false_branch->getNo();
    fprintf(yyout, "  br %s %s, label %%B%d, label %%B%d\n", type.c_str(),
            cond.c_str(), true_label, false_label);
}

void CondBrInstruction::setFalseBranch(BasicBlock* bb) {
    false_branch = bb;
}

BasicBlock* CondBrInstruction::getFalseBranch() {
    return false_branch;
}

void CondBrInstruction::setTrueBranch(BasicBlock* bb) {
    true_branch = bb;
}

BasicBlock* CondBrInstruction::getTrueBranch() {
    return true_branch;
}

RetInstruction::RetInstruction(Operand* src, BasicBlock* insert_bb)
    : Instruction(RET, insert_bb) {
    if (src != nullptr) {
        operands.push_back(src);
        src->addUse(this);
    }
}
void RetInstruction::replaceDef(Operand* new_) {
    if (operands.size()) {
        operands[0]->removeDef(this);
        operands[0] = new_;
        new_->setDef(this);
    }
}
RetInstruction::~RetInstruction() {
    if (!operands.empty())
        operands[0]->removeUse(this);
}

void RetInstruction::output() const {
    if (operands.empty()) {
        fprintf(yyout, "  ret void\n");
    } else {
        std::string ret, type;
        ret = operands[0]->toStr();
        type = operands[0]->getType()->toStr();
        fprintf(yyout, "  ret %s %s\n", type.c_str(), ret.c_str());
    }
}

AllocaInstruction::AllocaInstruction(Operand* dst,
                                     SymbolEntry* se,
                                     BasicBlock* insert_bb)
    : Instruction(ALLOCA, insert_bb) {
    operands.push_back(dst);
    dst->setDef(this);
    this->se = se;
}
void AllocaInstruction::replaceDef(Operand* new_) {
    operands[0]->removeDef(this);
    operands[0] = new_;
    new_->setDef(this);
}
AllocaInstruction::~AllocaInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
}


void AllocaInstruction::outputIntAlloca(std::string& dst, std::string& type)const {
    
    fprintf(yyout, "  %s = alloca %s, align 4\n", dst.c_str(), type.c_str());
}

void AllocaInstruction::outputArrayAlloca(std::string& dst, std::string& type)const {
    
    fprintf(yyout, "  %s = alloca %s, align 4\n", dst.c_str(), type.c_str());
}
//实现一个类AllocaInstruction的output()函数
//目的是将一个Alloca指令输出到标准输出流中，并且格式化输出使得它能被LLVM识别。
void AllocaInstruction::output() const {
    std::string dst, type;//"dst"是分配的内存位置，"type"是分配的数据类型。
    dst = operands[0]->toStr();//使用operands[0]->toStr()获取dst的值，这是目标寄存器或内存位置。
    type = se->getType()->toStr();
    if (se->getType()->isInt()) {
        this->outputIntAlloca(dst, type);
    } else if (se->getType()->isArray()) {
        this->outputArrayAlloca(dst, type);
    }
    
}

LoadInstruction::LoadInstruction(Operand* dst,
                                 Operand* src_addr,
                                 BasicBlock* insert_bb)
    : Instruction(LOAD, insert_bb) {
    operands.push_back(dst);
    operands.push_back(src_addr);
    dst->setDef(this);
    src_addr->addUse(this);
}
void LoadInstruction::replaceDef(Operand* new_) {
    operands[0]->removeDef(this);
    operands[0] = new_;
    new_->setDef(this);
}
LoadInstruction::~LoadInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
}

void LoadInstruction::output() const {
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    std::string src_type;
    std::string dst_type;
    dst_type = operands[0]->getType()->toStr();
    src_type = operands[1]->getType()->toStr();
    fprintf(yyout, "  %s = load %s, %s %s, align 4\n", dst.c_str(),
            dst_type.c_str(), src_type.c_str(), src.c_str());
}

StoreInstruction::StoreInstruction(Operand* dst_addr,
                                   Operand* src,
                                   BasicBlock* insert_bb)
    : Instruction(STORE, insert_bb) {
    operands.push_back(dst_addr);
    operands.push_back(src);
    dst_addr->addUse(this);
    src->addUse(this);
}

StoreInstruction::~StoreInstruction() {
    operands[0]->removeUse(this);
    operands[1]->removeUse(this);
}

void StoreInstruction::output() const {
    std::string dst = operands[0]->toStr();
    std::string src = operands[1]->toStr();
    std::string dst_type = operands[0]->getType()->toStr();
    std::string src_type = operands[1]->getType()->toStr();

    fprintf(yyout, "  store %s %s, %s %s, align 4\n", src_type.c_str(),
            src.c_str(), dst_type.c_str(), dst.c_str());
}

//dynamic_cast是C++中的运行时类型识别（RTTI），用于在运行时将基类的指针转换为派生类的指针。
//对于r0-r3的参数，直接返回寄存器编号。
//对于r4之后的参数，在调用这个函数之前，先加载它们到一个临时寄存器中，再返回这个临时寄存器。
//处理参数的寄存器分配，因为参数的个数是固定的，所以只需要考虑前4个参数的寄存器分配，对于第4个以后的参数，可以用r3来代替，或者使用其它方法来处理。

MachineOperand* Instruction::genMachineOperand(Operand* ope) {
    auto se = ope->getEntry();
    MachineOperand* mope = nullptr;
    if (se->isConstant())
        mope = new MachineOperand(
            MachineOperand::IMM,
            dynamic_cast<ConstantSymbolEntry*>(se)->getValue());
    else if (se->isTemporary())
        mope = new MachineOperand(
            MachineOperand::VREG,
            dynamic_cast<TemporarySymbolEntry*>(se)->getLabel());
    else if (se->isVariable()) {
        auto id_se = dynamic_cast<IdentifierSymbolEntry*>(se);
        if (id_se->isGlobal())
            mope = new MachineOperand(id_se->toStr().c_str());
        else if (id_se->isParam()) {
         
            if (id_se->getParamNo() >= 4)
		mope = new MachineOperand(MachineOperand::REG, 3);
               
            else
		mope = new MachineOperand(MachineOperand::REG,
                                          id_se->getParamNo());
            
            
               
        } else
            exit(0);
    }
    return mope;
}

MachineOperand* Instruction::genMachineReg(int reg) {
    return new MachineOperand(MachineOperand::REG, reg);
}

MachineOperand* Instruction::genMachineVReg() {
    return new MachineOperand(MachineOperand::VREG, SymbolTable::getLabel());
}

MachineOperand* Instruction::genMachineImm(int val) {
    return new MachineOperand(MachineOperand::IMM, val);
}

MachineOperand* Instruction::genMachineLabel(int block_no) {
    std::ostringstream buf;
    buf << ".L" << block_no;
    std::string label = buf.str();
    return new MachineOperand(label);
}
//在分配栈空间时，首先通过计算变量的大小，然后将这个大小除以 8，得到字节数，再将这个字节数作为参数传递给cur_func->AllocSpace()函数。这样做是为了确保变量的大小是字节为单位，以便在分配栈空间时使用。

//如果变量的大小小于0，则将其设置为4。这意味着如果变量的大小为负数，则它会被视为4字节。

//然后，使用dynamic_cast将operands[0]的符号表条目转换为TemporarySymbolEntry类型，并将偏移量设置为负偏移量，这样就可以将其存储在栈中。
void AllocaInstruction::genMachineCode(AsmBuilder* builder) {
    /* HINT:
     * Allocate stack space for local variabel
     * Store frame offset in symbol entry */
    auto cur_func = builder->getFunction();
    int size = se->getType()->getSize() / 8;
    if (size < 0)
        size = 4;
    int offset = cur_func->AllocSpace(size);
    dynamic_cast<TemporarySymbolEntry*>(operands[0]->getEntry())
        ->setOffset(-offset);
}


void LoadInstruction::genMachineCode(AsmBuilder* builder) {
    auto cur_block = builder->getBlock();
    MachineInstruction* cur_inst = nullptr;
    // Load global operand
    if (operands[1]->getEntry()->isVariable() &&
        dynamic_cast<IdentifierSymbolEntry*>(operands[1]->getEntry())
            ->isGlobal()) {
        auto dst = genMachineOperand(operands[0]);
        auto internal_reg1 = genMachineVReg();
        auto internal_reg2 = new MachineOperand(*internal_reg1);
        auto src = genMachineOperand(operands[1]);
        // example: load r0, addr_a
        cur_inst = new LoadMInstruction(cur_block, internal_reg1, src);
        cur_block->InsertInst(cur_inst);
        // example: load r1, [r0]
        cur_inst = new LoadMInstruction(cur_block, dst, internal_reg2);
        cur_block->InsertInst(cur_inst);
    }
    // Load local operand
    else if (operands[1]->getEntry()->isTemporary() && operands[1]->getDef() &&
             operands[1]->getDef()->isAlloc()) {
        // example: load r1, [r0, #4]
        auto dst = genMachineOperand(operands[0]);
        auto src1 = genMachineReg(11);
        auto off = dynamic_cast<TemporarySymbolEntry*>(operands[1]->getEntry())
                      ->getOffset();
        auto src2 = genMachineImm(off);
	//如果读取局部变量，会先判断局部变量的偏移量是否大于255或者小于-255，如果是，则会先将偏移量加载到虚拟寄存器中，再通过虚拟寄存器加载局部变量。
        if (!(off>=-255&&off<=255)) {
            auto operand = genMachineVReg();
            cur_block->InsertInst(
                (new LoadMInstruction(cur_block, operand, src2)));
            src2 = operand;
        }
        cur_inst = new LoadMInstruction(cur_block, dst, src1, src2);
        cur_block->InsertInst(cur_inst);
    }
    // Load operand from temporary variable
    else {
        // example: load r1, [r0]
        auto dst = genMachineOperand(operands[0]);
        auto src = genMachineOperand(operands[1]);
        cur_inst = new LoadMInstruction(cur_block, dst, src);
        cur_block->InsertInst(cur_inst);
    }
}
//这段代码实现了存储指令的机器码生成。它接受一个AsmBuilder类型的参数，表示汇编建造器。
//第一种情况是判断当前操作数1是否为常量，如果是常量则新建一个临时寄存器dst1，然后将操作数1对应的常量值装入dst1，并将src赋值为dst1，这样后面的存储操作就能使用这个临时寄存器来存储数据了。
//第二种情况存储到局部变量,如果第一个操作数是临时变量并且它有定义并且它是分配的，那么它就是局部变量。它将使用一个基于栈的地址（寄存器11+偏移量）来存储这个值。
//第三种情况是在将值存储到全局变量。首先，代码会创建一个临时虚拟寄存器（internal_reg1）。然后，将地址加载到该虚拟寄存器，这个地址来自全局变量的地址(dst)。接着，使用该临时虚拟寄存器作为目标地址，将值（src）存储到全局变量。
void StoreInstruction::genMachineCode(AsmBuilder* builder) {
    auto cur_block = builder->getBlock();
    MachineInstruction* cur_inst = nullptr;
    auto dst = genMachineOperand(operands[0]);
    auto src = genMachineOperand(operands[1]);
    // store immediate
    if (operands[1]->getEntry()->isConstant()) {
        auto dst1 = genMachineVReg();
        cur_inst = new LoadMInstruction(cur_block, dst1, src);
        cur_block->InsertInst(cur_inst);
        // src = dst1;
        src = new MachineOperand(*dst1);
    }
    int temp;
    if (operands[0]->getEntry()->isTemporary() && operands[0]->getDef() &&
        operands[0]->getDef()->isAlloc())temp = 0;
    else 
    {
        if (operands[0]->getEntry()->isVariable() &&
            dynamic_cast<IdentifierSymbolEntry*>(operands[0]->getEntry())
            ->isGlobal())temp = 1;
        else 
        {
            if (operands[0]->getType()->isPtr())temp = 2;
        }
    }
    switch (temp) {
    case 0:
        
    {
        auto src1 = genMachineReg(11);
        int off = dynamic_cast<TemporarySymbolEntry*>(operands[0]->getEntry())
            ->getOffset();
        auto src2 = genMachineImm(off);
        if (!(off >= -255 && off <= 255)) {
            auto operand = genMachineVReg();
            cur_block->InsertInst(
                new LoadMInstruction(cur_block, operand, src2));
            src2 = operand;
        }
        cur_block->InsertInst(new StoreMInstruction(cur_block, src, src1, src2));
    }
    break;
    case 1:
        
    {
        auto internal_reg1 = genMachineVReg();
        cur_block->InsertInst(new LoadMInstruction(cur_block, internal_reg1, dst));
        cur_block->InsertInst(new StoreMInstruction(cur_block, src, internal_reg1));
    }
    break;
    case 2:
        
    {
        cur_block->InsertInst(new StoreMInstruction(cur_block, src, dst));
    }
    break;
    
}
 }   

//它首先从生成器中取得当前块，然后从genMachineOperand中取得三个操作数。
//然后它检查是否有立即值,如果有，它会创建一个新的虚拟寄存器，并使用LoadMInstruction将立即值加载到该寄存器中。然后它会检查opcode，以确定需要生成哪种类型的二元指令。
//确认参数后，它会生成一个BinaryMInstruction，并将其插入到当前块中，这样就可以在AsmBuilder中生成机器码了。
void BinaryInstruction::genMachineCode(AsmBuilder* builder) {
    // complete other instructions
    
    auto dst = genMachineOperand(operands[0]);
    auto src1 = genMachineOperand(operands[1]);
    auto src2 = genMachineOperand(operands[2]);
    auto cur_block = builder->getBlock();

    MachineInstruction* cur_inst = nullptr;
    int tem = -1;
    if (src1->isImm()){
	auto internal_reg = genMachineVReg();
        cur_block->InsertInst(new LoadMInstruction(cur_block, internal_reg, src1));
        src1 = new MachineOperand(*internal_reg);}
    if (src2->isImm()){
	if ((opcode <= BinaryInstruction::OR &&
             ((ConstantSymbolEntry*)(operands[2]->getEntry()))->getValue() >
                 255) ||
            opcode >= BinaryInstruction::MUL)
            
        {
            auto internal_reg = genMachineVReg();
            cur_block->InsertInst(new LoadMInstruction(cur_block, internal_reg, src2));
            src2 = new MachineOperand(*internal_reg);
        }}
    
   
    switch (opcode) {
    case ADD:
        cur_inst = new BinaryMInstruction(
            cur_block, BinaryMInstruction::ADD, dst, src1, src2);
        break;
    case SUB:
        cur_inst = new BinaryMInstruction(
            cur_block, BinaryMInstruction::SUB, dst, src1, src2);
        break;
    case AND:
        cur_inst = new BinaryMInstruction(
            cur_block, BinaryMInstruction::AND, dst, src1, src2);
        break;
    case OR:
        cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::OR,
            dst, src1, src2);
        break;
    case MUL:
        cur_inst = new BinaryMInstruction(
            cur_block, BinaryMInstruction::MUL, dst, src1, src2);
        break;
    case DIV:
        cur_inst = new BinaryMInstruction(
            cur_block, BinaryMInstruction::DIV, dst, src1, src2);
        break;
    case MOD: {
        cur_inst = new BinaryMInstruction(
            cur_block, BinaryMInstruction::DIV, dst, src1, src2);
        MachineOperand* dst1 = new MachineOperand(*dst);
        src1 = new MachineOperand(*src1);
        src2 = new MachineOperand(*src2);
        cur_block->InsertInst(cur_inst);
        // c = c * b
        cur_inst = new BinaryMInstruction(
            cur_block, BinaryMInstruction::MUL, dst1, dst, src2);
        cur_block->InsertInst(cur_inst);
        dst = new MachineOperand(*dst1);
        // c = a - c
        cur_inst = new BinaryMInstruction(
            cur_block, BinaryMInstruction::SUB, dst, src1, dst1);
        break;
    }
    default:
        break;
    }
    cur_block->InsertInst(cur_inst);
}
//CmpInstruction::genMachineCode 函数是用来生成比较指令的函数，其中opcode是指令的码，operands是操作的操作数，genMachineOperand是用来生成操作数的机器指令的函数，AsmBuilder用来存储指令，genMachineVReg用来生成虚拟寄存器，genMachineImm用来生成立即数。
//首先判断操作数src1是否为立即数，如果是立即数则使用loadM指令将立即数加载到寄存器中，然后判断操作数src2是否为大于255的立即数，如果是则使用loadM指令将立即数加载到寄存器中，最后根据opcode生成相应的比较机器指令，将指令添加到AsmBuilder中，如果比较的opcode是大于等于L小于等于GE，则还需要根据opcode生成相应的赋值指令，将指令添加到AsmBuilder中。
void CmpInstruction::genMachineCode(AsmBuilder* builder) {
    auto cur_block = builder->getBlock();
    auto src1 = genMachineOperand(operands[1]);
    auto src2 = genMachineOperand(operands[2]);
    MachineInstruction* cur_inst = nullptr;
    int tem=-1;
    if (src1->isImm()){
	// if source 1 is an immediate, we need to load it into a register
        auto internal_reg = genMachineVReg();
        cur_block->InsertInst(new LoadMInstruction(cur_block, internal_reg, src1));
        src1 = new MachineOperand(*internal_reg);}
    if(src2->isImm()){
	// if source 2 is a large immediate, we need to load it into a register
        // (large imm's won't fit in a single instruction)
        if(((ConstantSymbolEntry*)(operands[2]->getEntry()))->getValue() > 255)
        {
            auto internal_reg = genMachineVReg();
            cur_block->InsertInst(new LoadMInstruction(cur_block, internal_reg, src2));
            src2 = new MachineOperand(*internal_reg);
        }
    }
    
    // generate the comparison instruction
    cur_block->InsertInst(new CmpMInstruction(cur_block, src1, src2, opcode));
    // if this is a comparison with a result, generate the movs to set the result
    if (opcode >= CmpInstruction::L && opcode <= CmpInstruction::GE) {
        auto dst = genMachineOperand(operands[0]);
        auto falseOperand = genMachineImm(0);
        auto trueOperand = genMachineImm(1);
        
        cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, dst,
            trueOperand, opcode));
        cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, dst,
            falseOperand, 7 - opcode));
    }
}

void UncondBrInstruction::genMachineCode(AsmBuilder* builder) {
	auto cur_block = builder->getBlock();
	auto dst = genMachineLabel(branch->getNo());//一条无条件跳转指令即可
	cur_block->InsertInst(new BranchMInstruction(cur_block, BranchMInstruction::B, dst));


}
void CondBrInstruction::genMachineCode(AsmBuilder* builder) {
    auto cur_block = builder->getBlock();
    auto dst = genMachineLabel(true_branch->getNo());
    cur_block->InsertInst(new BranchMInstruction(cur_block, BranchMInstruction::B,
        dst, cur_block->getCmpCond()));
    dst = genMachineLabel(false_branch->getNo());

    cur_block->InsertInst(new BranchMInstruction(cur_block, BranchMInstruction::B, dst));
}

void RetInstruction::genMovInstruction(AsmBuilder* builder) {
    auto cur_block = builder->getBlock();
    if (!operands.empty()) {
        auto dst = new MachineOperand(MachineOperand::REG, 0);
        auto src = genMachineOperand(operands[0]);
        auto cur_inst =
            new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src);
        cur_block->InsertInst(cur_inst);
    }
}

void RetInstruction::genRestoreInstruction(AsmBuilder* builder,MachineBlock* cur_block) {
    auto cur_func = builder->getFunction();
    auto sp = new MachineOperand(MachineOperand::REG, 13);
    auto size =
        new MachineOperand(MachineOperand::IMM, cur_func->AllocSpace(0));
    auto cur_inst = new BinaryMInstruction(cur_block, BinaryMInstruction::ADD,
        sp, sp, size);
    cur_block->InsertInst(cur_inst);
    // auto fp = new MachineOperand(MachineOperand::REG, 11);
    // auto cur_inst1 = new StackMInstrcuton(cur_block, StackMInstrcuton::POP,
    //                                       cur_func->getSavedRegs(), fp);
    // cur_block->InsertInst(cur_inst1);
}

void RetInstruction::genBxInstruction(AsmBuilder* builder) {
    auto cur_block = builder->getBlock();
    auto lr = new MachineOperand(MachineOperand::REG, 14);
    auto cur_inst2 =
        new BranchMInstruction(cur_block, BranchMInstruction::BX, lr);
    cur_block->InsertInst(cur_inst2);
}
void RetInstruction::genMachineCode(AsmBuilder* builder) {
    auto cur_block = builder->getBlock();
    // generate mov instruction
    this->genMovInstruction(builder);

    // restore callee saved registers and sp, fp
    this->genRestoreInstruction(builder,cur_block);

    // generate bx instruction
    this->genBxInstruction(builder);
}
void CallInstruction::replaceDef(Operand* new_) {
    if (dst) {
        operands[0]->removeDef(this);
        operands[0] = new_;
        new_->setDef(this);
        dst = new_;
    }
}

//首先，它首先打印出一个空格，以使输出的指令更加美观；
//然后，它根据函数调用指令是否有返回值，决定是否打印出返回值；
//接着，它根据函数调用指令的参数个数，动态地打印出函数参数；
//最后，它打印出换行符，以结束函数调用指令的输出。
void CallInstruction::output() const {
    fprintf(yyout, "  ");
    if (operands[0])
        fprintf(yyout, "%s = ", operands[0]->toStr().c_str());
    FunctionType* type = (FunctionType*)(func->getType());
    fprintf(yyout, "call %s %s(", type->getRetType()->toStr().c_str(),
            func->toStr().c_str());
    for (long unsigned int i = 1; i < operands.size(); i++) {
        if (i != 1)
            fprintf(yyout, ", ");
        fprintf(yyout, "%s %s", operands[i]->getType()->toStr().c_str(),
                operands[i]->toStr().c_str());
    }
    fprintf(yyout, ")\n");
}



void ZextInstruction::replaceDef(Operand* new_) {
    operands[0]->removeDef(this);
    operands[0] = new_;
    new_->setDef(this);
}


void ZextInstruction::output() const {
    Operand* dst = operands[0];
    Operand* src = operands[1];
    fprintf(yyout, "  %s = zext %s %s to i32\n", dst->toStr().c_str(),
            src->getType()->toStr().c_str(), src->toStr().c_str());
}



void XorInstruction::replaceDef(Operand* new_) {
    operands[0]->removeDef(this);
    operands[0] = new_;
    new_->setDef(this);
}

void XorInstruction::output() const {
    Operand* dst = operands[0];
    Operand* src = operands[1];
    fprintf(yyout, "  %s = xor %s %s, true\n", dst->toStr().c_str(),
            src->getType()->toStr().c_str(), src->toStr().c_str());
}





GepInstruction::GepInstruction(Operand* dst,
                               Operand* arr,
                               Operand* idx,
                               BasicBlock* insert_bb,
                               bool paramFirst)
    : Instruction(GEP, insert_bb), paramFirst(paramFirst) {
    operands.push_back(dst);
    operands.push_back(arr);
    operands.push_back(idx);
    dst->setDef(this);
    arr->addUse(this);
    idx->addUse(this);
    first = false;
    init = nullptr;
    last = false;
}
void GepInstruction::replaceDef(Operand* new_) {
    operands[0]->removeDef(this);
    operands[0] = new_;
    new_->setDef(this);
}
// GepInstruction类的output函数实现：
// 根据第一个参数和最后一个参数是什么，来选择不同形式的getelementptr语句生成
// 位置0的参数是存储变量，即将赋值给getelementptr表达式的结果
// 位置1的参数是数组变量
// 位置2的参数是索引变量
void GepInstruction::output() const {
    Operand *dst = operands[0], *arr = operands[1], *idx = operands[2];
    std::string arrType = arr->getType()->toStr();

    if (paramFirst)
        fprintf(yyout, "  %s = getelementptr inbounds %s, %s %s, i32 %s\n",
                dst->toStr().c_str(),
                arrType.substr(0, arrType.size() - 1).c_str(), arrType.c_str(),
                arr->toStr().c_str(), idx->toStr().c_str());
    else
        fprintf(
            yyout, "  %s = getelementptr inbounds %s, %s %s, i32 0, i32 %s\n",
            dst->toStr().c_str(), arrType.substr(0, arrType.size() - 1).c_str(),
            arrType.c_str(), arr->toStr().c_str(), idx->toStr().c_str());
}

GepInstruction::~GepInstruction() {
    Operand *dst = operands[0], *arr = operands[1], *idx = operands[2];
    dst->setDef(nullptr);
    if (dst->usersNum() == 0)
        delete dst;
    arr->removeUse(this);
    idx->removeUse(this);
}

//首先会生成调用指令中函数名后面指定的操作数。然后使用这些操作数生成MOV和PUSH等指令。还会生成Branch指令，跳转到被调用函数的标签。最后，如果指定了目标，将会生成一个MOV指令，将函数的返回值分配给它。
//这段代码的控制流可以分为三个部分。首先，会循环地从operands数组中取出操作数，并生成机器代码来处理它们。其次，会生成Branch指令，跳转到被调用函数的标签。最后，如果指定了目标，将会生成一个MOV指令，将函数的返回值分配给它。
void CallInstruction::genMachineCode(AsmBuilder* builder) {
    auto cur_block = builder->getBlock();
    MachineOperand* operand;  //, *num;
    MachineInstruction* cur_inst;
    // auto fp = new MachineOperand(MachineOperand::REG, 11);
    int idx = 0;
    //operand是一个机器操作数，可以是一个寄存器,内存地址或立即数等。operands是一个操作数数组，存储指令的参数。
    //从operands数组中取出操作数，并生成机器指令来处理它们；
    for (auto it = operands.begin(); it < operands.end(); it++, idx++) {
	//idx不能等于5是因为operands数组中只有4个元素，因此idx不能超过4，否则程序会出错
        
        if (idx == 5)
            break;
	if (idx == 0)
            continue;
        operand = genMachineReg(idx - 1);
        auto src = genMachineOperand(operands[idx]);
        if (src->isImm() && src->getVal() > 255) {
	    cur_block->InsertInst(new LoadMInstruction(cur_block, operand, src));
        } else
	    cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV,
                                           operand, src));
        
    }
    //从operands数组中取出操作数，并将其压入堆栈中，以便被调用函数使用
    int i = operands.size() - 1;
    for (i ; i > 4; i--) {
	//这行代码用于生成操作数，即从operands数组中取出第i个操作数，并用genMachineOperand方法将其转换成机器操作数operand。
        operand = genMachineOperand(operands[i]);
	std::vector<MachineOperand*> vec;
        if (operand->isImm()) {
            auto dst = genMachineVReg();
            if (operand->getVal() < 256)
		cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV,
                                               dst, operand));
            else
                cur_block->InsertInst(new LoadMInstruction(cur_block, dst, operand));
            operand = dst;
        }
        
        cur_block->InsertInst(new StackMInstrcuton(cur_block, StackMInstrcuton::PUSH, vec,
                                        operand));
    }
    auto label = new MachineOperand(func->toStr().c_str());
    cur_block->InsertInst(new BranchMInstruction(cur_block, BranchMInstruction::BL, label));
    if (operands.size() > 5) {
        auto off = genMachineImm((operands.size() - 5) * 4);
        auto sp = new MachineOperand(MachineOperand::REG, 13);
        cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD,
                                          sp, sp, off));
    }
    if (dst) {
        operand = genMachineOperand(dst);
        auto r0 = new MachineOperand(MachineOperand::REG, 0);
        cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, operand, r0));
    }
}

void ZextInstruction::genMachineCode(AsmBuilder* builder) {
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(operands[0]);
    auto src = genMachineOperand(operands[1]);
    cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, dst, src));
}

void XorInstruction::genMachineCode(AsmBuilder* builder) {
    auto cur_block = builder->getBlock();
    auto dst = genMachineOperand(operands[0]);
    auto trueOperand = genMachineImm(1);
    auto falseOperand = genMachineImm(0);
    cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, dst,
                                        trueOperand, MachineInstruction::EQ));
    cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, dst,
                                   falseOperand, MachineInstruction::NE));
}

void GepInstruction::genMachineCode(AsmBuilder* builder) {
    auto cur_block = builder->getBlock();
    MachineInstruction* cur_inst;
    auto dst = genMachineOperand(operands[0]);
    auto idx = genMachineOperand(operands[2]);
    if(init){
        if(last){
            auto base = genMachineOperand(init);
            cur_block->InsertInst(new BinaryMInstruction(
                cur_block, BinaryMInstruction::ADD, dst, base, genMachineImm(4)));
        }
        return;
    }
    MachineOperand* base = nullptr;
    int size;
    auto idx1 = genMachineVReg();
    if (idx->isImm()) {
        if (idx->getVal() < 255) {
	    cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, idx1, idx));
        } else {
	    cur_block->InsertInst(new LoadMInstruction(cur_block, idx1, idx));
        }
        idx = new MachineOperand(*idx1);
    }
    if (paramFirst) {
        size =
            ((PointerType*)(operands[1]->getType()))->getType()->getSize() / 8;
    } else {
        if (first) {
            base = genMachineVReg();
            if (operands[1]->getEntry()->isVariable() &&
                ((IdentifierSymbolEntry*)(operands[1]->getEntry()))
                    ->isGlobal()) {
                auto src = genMachineOperand(operands[1]);
                cur_block->InsertInst(new LoadMInstruction(cur_block, base, src));
            } else {
                int offset = ((TemporarySymbolEntry*)(operands[1]->getEntry()))
                                 ->getOffset();
                if (offset > -255 && offset < 255) {
                cur_block->InsertInst( new MovMInstruction(cur_block, MovMInstruction::MOV,
                                            base, genMachineImm(offset)));
                } else {
                    cur_block->InsertInst(new LoadMInstruction(cur_block, base,
                                                    genMachineImm(offset)));
                }
            }
        }
        ArrayType* type =
            (ArrayType*)(((PointerType*)(operands[1]->getType()))->getType());
        size = type->getElementType()->getSize() / 8;
    }
    auto size1 = genMachineVReg();
    if (size > -255 && size < 255) {
        cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, size1,
                                       genMachineImm(size)));
    } else {
        cur_block->InsertInst(new LoadMInstruction(cur_block, size1, genMachineImm(size)));
    }
    auto off = genMachineVReg();
    off = new MachineOperand(*off);
    cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::MUL, off,
                                      idx, size1));
    if (paramFirst || !first) {
        auto arr = genMachineOperand(operands[1]);
        cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD,
                                          dst, arr, off));
    } else {
        auto addr = genMachineVReg();
        auto base1 = new MachineOperand(*base);
        cur_block->InsertInst(new BinaryMInstruction(cur_block, BinaryMInstruction::ADD,
                                          addr, base1, off));
        addr = new MachineOperand(*addr);
        if (operands[1]->getEntry()->isVariable() &&
            ((IdentifierSymbolEntry*)(operands[1]->getEntry()))->isGlobal()) {
            cur_block->InsertInst(new MovMInstruction(cur_block, MovMInstruction::MOV, dst, addr));
        } else {
            auto fp = genMachineReg(11);
            cur_block->InsertInst(new BinaryMInstruction(
                cur_block, BinaryMInstruction::ADD, dst, fp, addr));
        }

    }
}
Instruction* StoreInstruction::copy() {
    return new StoreInstruction(*this);
}


