#ifndef __INSTRUCTION_H__
#define __INSTRUCTION_H__

#include <map>
#include <sstream>
#include <vector>
#include "AsmBuilder.h"
#include "Operand.h"

class BasicBlock;

class Instruction {
   public:
    Instruction(unsigned instType, BasicBlock* insert_bb = nullptr);
    virtual ~Instruction();
    BasicBlock* getParent();
    bool isUncond() const { return instType == UNCOND; };
    bool isCond() const { return instType == COND; };
    bool isAlloc() const { return instType == ALLOCA; };
    bool isRet() const { return instType == RET; };
    void setParent(BasicBlock*);
    void setNext(Instruction*);
    void setPrev(Instruction*);
    Instruction* getNext();
    bool isCall() const { return instType == CALL; }
    bool isStore() const { return instType == STORE; }
    bool isLoad() const { return instType == LOAD; };
    bool isCmp() const { return instType == CMP; };
    bool isXor() const { return instType == XOR; };
    Instruction* getPrev();
    virtual void output() const = 0;
    MachineOperand* genMachineOperand(Operand*);
    MachineOperand* genMachineReg(int reg);
    MachineOperand* genMachineVReg();
    MachineOperand* genMachineImm(int val);
    MachineOperand* genMachineLabel(int block_no);
    virtual void genMachineCode(AsmBuilder*) = 0;
    int getInstType() { return instType; }
virtual bool isConstExp() { return false; }
//更改
    Operand* getDef() { return operands[0]; }
    virtual std::vector<Operand*> getUse() { return std::vector<Operand*>(); }
    virtual void replaceUse(Operand* old, Operand* new_) {}
    virtual void replaceDef(Operand* new_) {}
    bool hasEqualOp(Instruction* in) const {
        if ((int)instType == in->getInstType())
            if ((int)opcode == in->getOpcode())
                return true;
        return false;
    }
    int getOpcode() const { return opcode; }
    virtual Instruction* copy() = 0;
   protected:
    unsigned instType;
    unsigned opcode;
    Instruction* prev;
    
    Instruction* next;
    
    
    BasicBlock* parent;
    std::vector<Operand*> operands;
    enum {
        BINARY,
        COND,
        UNCOND,
        RET,
        LOAD,
        STORE,
        CMP,
        ALLOCA,
        CALL,
        ZEXT,
        XOR,
        GEP
    };
    
};

// meaningless instruction, used as the head node of the instruction list.
class DummyInstruction : public Instruction {
   public:
    DummyInstruction() : Instruction(-1, nullptr){};
    void output() const {};
    void genMachineCode(AsmBuilder*){};
};

class AllocaInstruction : public Instruction {
   public:
    AllocaInstruction(Operand* dst,
                      SymbolEntry* se,
                      BasicBlock* insert_bb = nullptr);
    ~AllocaInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    //更改
    Operand* getDef() { return operands[0]; }
    void outputIntAlloca(std::string& , std::string& )const;
    void replaceDef(Operand* new_);
    void outputArrayAlloca(std::string& , std::string& )const;
   private:
    SymbolEntry* se;
};

class LoadInstruction : public Instruction {
   public:
    LoadInstruction(Operand* dst,
                    Operand* src_addr,
                    BasicBlock* insert_bb = nullptr);
    ~LoadInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    void replaceDef(Operand* new_);
//更改
Operand* getDef() { return operands[0]; }
 std::vector<Operand*> getUse() {
        return std::vector<Operand*>({operands[1]});
    }
};

class StoreInstruction : public Instruction {
   public:
    StoreInstruction(Operand* dst_addr,
                     Operand* src,
                     BasicBlock* insert_bb = nullptr);
    ~StoreInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
std::vector<Operand*> getUse() {
        return std::vector<Operand*>({operands[0], operands[1]});
    }
    Instruction* copy();
};

class BinaryInstruction : public Instruction {
   public:
    BinaryInstruction(unsigned opcode,
                      Operand* dst,
                      Operand* src1,
                      Operand* src2,
                      BasicBlock* insert_bb = nullptr);
    ~BinaryInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    enum { SUB, ADD, AND, OR, MUL, DIV, MOD };
//更改
    Operand* getDef() { return operands[0]; }
void replaceDef(Operand* new_);
std::vector<Operand*> getUse() {
        return std::vector<Operand*>({operands[1], operands[2]});
    }
};

class CmpInstruction : public Instruction {
   public:
    CmpInstruction(unsigned opcode,
                   Operand* dst,
                   Operand* src1,
                   Operand* src2,
                   BasicBlock* insert_bb = nullptr);
    ~CmpInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    enum { E, NE, L, LE, G, GE };
void replaceDef(Operand* new_);
//更改
    Operand* getDef() { return operands[0]; }
 std::vector<Operand*> getUse() {
        return std::vector<Operand*>({operands[1], operands[2]});
    }
};

// unconditional branch
class UncondBrInstruction : public Instruction {
   public:
    UncondBrInstruction(BasicBlock*, BasicBlock* insert_bb = nullptr);
    void output() const;
    void setBranch(BasicBlock*);
    BasicBlock* getBranch();
    void genMachineCode(AsmBuilder*);

   protected:
    BasicBlock* branch;
};

// conditional branch
class CondBrInstruction : public Instruction {
   public:
    CondBrInstruction(BasicBlock*,
                      BasicBlock*,
                      Operand*,
                      BasicBlock* insert_bb = nullptr);
    ~CondBrInstruction();
    void output() const;
    void setTrueBranch(BasicBlock*);
    BasicBlock* getTrueBranch();
    void setFalseBranch(BasicBlock*);
    BasicBlock* getFalseBranch();
    void genMachineCode(AsmBuilder*);
std::vector<Operand*> getUse() {
        return std::vector<Operand*>({operands[0]});
    }

   protected:
    BasicBlock* true_branch;
    BasicBlock* false_branch;
};

class RetInstruction : public Instruction {
   public:
    RetInstruction(Operand* src, BasicBlock* insert_bb = nullptr);
    ~RetInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    void genMovInstruction(AsmBuilder*);
    void genRestoreInstruction(AsmBuilder*,MachineBlock*);
    void replaceDef(Operand* new_);
    void genBxInstruction(AsmBuilder*);
    std::vector<Operand*> getUse() {
        if (operands.size())
            return std::vector<Operand*>({operands[0]});
        return std::vector<Operand*>();
    }
};

class CallInstruction : public Instruction {
   private:
    SymbolEntry* func;
    Operand* dst;

   public:
    CallInstruction(Operand* dst,
                                 SymbolEntry* func,
                                 std::vector<Operand*> params,
                                 BasicBlock* insert_bb)
    : Instruction(CALL, insert_bb), func(func), dst(dst) {
    operands.push_back(dst);
    if (dst)
        dst->setDef(this);
    for (auto param : params) {
        operands.push_back(param);
        param->addUse(this);
    }
};
    ~CallInstruction() {
    operands[0]->setDef(nullptr);
    if (!operands[0]->usersNum() )
        delete operands[0];
    for (long unsigned int i = 1; i < operands.size(); i++)
        operands[i]->removeUse(this);
}
    void output() const;
    void genMachineCode(AsmBuilder*);
void replaceDef(Operand* new_);
 std::vector<Operand*> getUse() {
        std::vector<Operand*> vec;
        for (auto it = operands.begin() + 1; it != operands.end(); it++)
            vec.push_back(*it);
        return vec;
    }
};

class ZextInstruction : public Instruction {
   public:
    ZextInstruction(Operand* dst,
                                 Operand* src,
                                 BasicBlock* insert_bb)
    : Instruction(ZEXT, insert_bb) {
    operands.push_back(dst);
    operands.push_back(src);
    dst->setDef(this);
    src->addUse(this);
};
    ~ZextInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
};
    void output() const;
    void genMachineCode(AsmBuilder*);
void replaceDef(Operand* new_);
std::vector<Operand*> getUse() {
        return std::vector<Operand*>({operands[1]});
    }
};

class XorInstruction : public Instruction {
   public:
    XorInstruction(Operand* dst,
                               Operand* src,
                               BasicBlock* insert_bb)
    : Instruction(XOR, insert_bb) {
    operands.push_back(dst);
    operands.push_back(src);
    dst->setDef(this);
    src->addUse(this);
};
   ~XorInstruction() {
    operands[0]->setDef(nullptr);
    if (operands[0]->usersNum() == 0)
        delete operands[0];
    operands[1]->removeUse(this);
};
    void output() const;
    void genMachineCode(AsmBuilder*);
void replaceDef(Operand* new_);
 std::vector<Operand*> getUse() {
        return std::vector<Operand*>({operands[1]});
    }
};

class GepInstruction : public Instruction {
   private:
    bool paramFirst;
    bool first;
    bool last;
    Operand* init;

   public:
    GepInstruction(Operand* dst,
                   Operand* arr,
                   Operand* idx,
                   BasicBlock* insert_bb = nullptr,
                   bool paramFirst = false);
    ~GepInstruction();
    void output() const;
    void genMachineCode(AsmBuilder*);
    void setFirst() { first = true; };
    void setLast() { last = true; };
    Operand* getInit() const { return init; };
void replaceDef(Operand* new_);
    void setInit(Operand* init) { this->init = init; };
    void genMachineMovInstruction(AsmBuilder* , MachineOperand* , MachineOperand*);
    void genMachineLoadInstruction(AsmBuilder*, MachineOperand* , Operand* );
 std::vector<Operand*> getUse() {
        return std::vector<Operand*>({operands[1], operands[2]});
    }

};

#endif
