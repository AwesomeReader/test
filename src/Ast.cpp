#include "IRBuilder.h"
#include "Instruction.h"
#include "SymbolTable.h"
#include "Type.h"
#include "Unit.h"
#include "Ast.h"
#include <stack>
#include <string>
#include<stdlib.h>
#include<cstring>
#include<vector>

extern Unit unit;
extern MachineUnit mUnit;

#include <iostream>
using namespace std;




#define get_block_parent builder->getInsertBB()->getParent()
#define F(b, c, d) ((b & c) | ((~b) & d))
#define G(b, c, d) ((b & d) | (c & (~d)))
#define H(b, c, d) (b ^ c ^ d)
#define I(b, c, d) (c ^ (b | (~d)))
extern FILE* yyout;
int num_of_group;	// 分组个数
unsigned int A, B, C, D;
int Node::counter = 0;
IRBuilder* Node::builder;

Node::Node() {
    seq = counter++;
    next = nullptr;
}

void Node::setNext(Node* node) {
    Node* n = this;
    while (n->getNext()) {
        n = n->getNext();
    }
    if (n == this) {
        this->next = node;
    } else {
        n->setNext(node);
    }
}

void Node::backPatch(std::vector<Instruction*>& list, BasicBlock* bb) {
    for (auto& inst : list) {
        if (inst->isCond())
            dynamic_cast<CondBrInstruction*>(inst)->setTrueBranch(bb);
        else if (inst->isUncond())
            dynamic_cast<UncondBrInstruction*>(inst)->setBranch(bb);
    }
}



std::vector<Instruction*> Node::merge(std::vector<Instruction*>& list1,
                                      std::vector<Instruction*>& list2) {
    std::vector<Instruction*> res(list1);
    res.insert(res.end(), list2.begin(), list2.end());
    return res;
}

void Ast::genCode(Unit* unit) {
    IRBuilder* builder = new IRBuilder(unit);
    Node::setIRBuilder(builder);
    root->genCode();
}

const unsigned S[64] =
{
	7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
	5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
	4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
	6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
};




void FunctionDef::genCode() {
    Unit* unit = builder->getUnit();
    Function* func = new Function(unit, se);
    BasicBlock* entry = func->getEntry();
    // set the insert point to the entry basicblock of this function.

    builder->setInsertBB(entry);
    if (decl)
        decl->genCode();
    // function中的stmt节点是用compoundstmt进行初始化的
    if (stmt)
        stmt->genCode();

    /**
     * Construct control flow graph. You need do set successors and predecessors
     * for each basic block. Todo
     */

    // BasicBlock* void_retbb=new BasicBlock(func);
    // new RetInstruction(nullptr, void_retbb);
    for (auto block = func->begin(); block != func->end(); block++) {
        //获取该块的最后一条指令
        Instruction* i = (*block)->begin();
        Instruction* last = (*block)->rbegin();
       
        while (i != last) 
        {
             //预定义一个判决因子
            auto judgement=(i->isCond() || i->isUncond());
            if (judgement) 
            {
                (*block)->remove(i);
            }
            i = i->getNext();
        }
        /*
        是这样的，最后一条指令只能是跳转指令或者函数返回指令，基本块中间不含有控制流指令
        那么如果最后一条指令是跳转指令的话，就会有两个后继节点，分别对应真假两个分支
        如果最后一条指令是无条件条转指令的话，就会对应一个后继
        如果最后一条指令是函数返回指令的话，就没有后继了
        */
        if (last->isCond()) 
        {   

            //进来的话说明你是有条件的跳，因此会对应两个分支
            BasicBlock *truebranch=dynamic_cast<CondBrInstruction*>(last)->getTrueBranch();
            BasicBlock *falsebranch=dynamic_cast<CondBrInstruction*>(last)->getFalseBranch();
         
                
            if (truebranch->empty()) 
            {
                new RetInstruction(nullptr, truebranch);

            } 
            else if (falsebranch->empty())
            {
                new RetInstruction(nullptr, falsebranch);
            }
            
            (*block)->addSucc(truebranch);
            (*block)->addSucc(falsebranch);
            truebranch->addPred(*block);
            falsebranch->addPred(*block);
        }
         else if (last->isUncond())  //无条件跳转指令可获取跳转的目标块
        {   
            //此时因为是无条件，故而只有一个后继
            BasicBlock* dst =dynamic_cast<UncondBrInstruction*>(last)->getBranch();
            (*block)->addSucc(dst);
            dst->addPred(*block);


            //预定义一个判决因子，用来查看是否非空
            auto judgement=dst->empty();
            if (judgement)
             {  
                //预定义一个判决因子1
                auto judgement1=(((FunctionType*)(se->getType()))->getRetType() == TypeSystem::intType);

                if (judgement1)
                    new RetInstruction(new Operand(new ConstantSymbolEntry(
                                           TypeSystem::intType, 0)),
                                       dst);
                else if (((FunctionType*)(se->getType()))->getRetType() ==
                         TypeSystem::voidType)
                    new RetInstruction(nullptr, dst);
            }

        }
        //最后一条语句是函数返回，那就肯定没有后继了。
        else if (!last->isRet()) 
        {
            if (((FunctionType*)(se->getType()))->getRetType() ==
                TypeSystem::voidType) 
            {
                new RetInstruction(nullptr, *block);
            }
        }
    }
}


const unsigned T[64] =
{
	0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
	0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
	0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
	0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
	0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
	0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
	0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
	0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
	0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
	0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
	0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
	0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
	0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
	0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
	0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
	0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
};

BinaryExpr::BinaryExpr(SymbolEntry* se,
                       int op,
                       ExprNode* expr1,
                       ExprNode* expr2)
    : ExprNode(se), op(op), expr1(expr1), expr2(expr2) {
    dst = new Operand(se);
    std::string op_str;
    switch (op) {
        case ADD:
            op_str = "+";
            break;
        case SUB:
            op_str = "-";
            break;
        case MUL:
            op_str = "*";
            break;
        case DIV:
            op_str = "/";
            break;
        case MOD:
            op_str = "%";
            break;
        case AND:
            op_str = "&&";
            break;
        case OR:
            op_str = "||";
            break;
        case LESS:
            op_str = "<";
            break;
        case LESSEQUAL:
            op_str = "<=";
            break;
        case GREATER:
            op_str = ">";
            break;
        case GREATEREQUAL:
            op_str = ">=";
            break;
        case EQUAL:
            op_str = "==";
            break;
        case NOTEQUAL:
            op_str = "!=";
            break;
    }

    //预定义两个判决因子，分别查看是否是void类型
    auto judgement3=expr1->getType()->isVoid();
    auto judgement4= expr2->getType()->isVoid();


    if (judgement3 ||judgement4) 
    {
        fprintf(stderr,
                "invalid operand of type \'void\' to binary \'opeartor%s\'\n",
                op_str.c_str());
    }
    if (op >= BinaryExpr::AND && op <= BinaryExpr::NOTEQUAL) {
        type = TypeSystem::boolType;
        if (op == BinaryExpr::AND || op == BinaryExpr::OR) {
            if (expr1->getType()->isInt() &&
                expr1->getType()->getSize() == 32) {
                ImplictCastExpr* temp = new ImplictCastExpr(expr1);
                this->expr1 = temp;
            }
            if (expr2->getType()->isInt() &&
                expr2->getType()->getSize() == 32) {
                ImplictCastExpr* temp = new ImplictCastExpr(expr2);
                this->expr2 = temp;
            }
        }
    } else
        type = TypeSystem::intType;
};


unsigned int* Padding(string message) {
	num_of_group = (message.length() + 8) / 64 + 1;
	unsigned int* result = new unsigned int[num_of_group * 16](); 
	// unsigned int 32位4字节
	int i=0;
	int	j = 0;
	for (i=0; i < 16; i++) {
		// 左移位直接填充到unsigned int对应的位置上就行
		result[j] |= (message[i] << ((i % 4) * 8));
		if ((i + 1) % 4 == 0) j++;
	}
	result[j] |= 0x80 << ((i % 4) * 8);	
	result[num_of_group * 16 - 2] = message.length() * 8;
	return result;
}

unsigned int CLS(unsigned int temp, unsigned int offset) {
	unsigned int result = temp << offset;
	// 充分利用移位的特点，直接填充至右侧
	result |= temp >> (32 - offset);
	return result;
}



void RealMD5(unsigned int* message) {
	unsigned int a = A, b = B, c = C, d = D;
	unsigned int g, k, t, temp;
	for (int i = 0; i < 16; i++) {
		g = F(b, c, d);
		k = i;
		t = b + CLS(a + g + message[k] + T[i], S[i]);
		temp = d;
		d = c;
		c = b;
		b = t;
		a = temp;
	}
	for (int i = 16; i < 32; i++) {
		g = G(b, c, d);
		k = (5 * i + 1) % 16;
		t = b + CLS(a + g + message[k] + T[i], S[i]);
		temp = d;
		d = c;
		c = b;
		b = t;
		a = temp;
	}
	for (int i = 32; i < 48; i++) {
		g = H(b, c, d);
		k = (3 * i + 5) % 16;
		t = b + CLS(a + g + message[k] + T[i], S[i]);
		temp = d;
		d = c;
		c = b;
		b = t;
		a = temp;
	}
	for (int i = 48; i < 64; i++) {
		g = I(b, c, d);
		k = (7 * i) % 16;
		t = b + CLS(a + g + message[k] + T[i], S[i]);
		temp = d;
		d = c;
		c = b;
		b = t;
		a = temp;
	}

	A += a;
	B += b;
	C += c;
	D += d;
}



void BinaryExpr::genCode() {
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();
    //这里要使用回填技术，因为需要一遍扫描，而归约的时候才能回写
    
    if (op == AND) {
        BasicBlock* true_branch = new BasicBlock(func); 
        expr1->genCode();//生成相应的中间代码
        backPatch(expr1->trueList(), true_branch);
        builder->setInsertBB(true_branch);  //当前指令插入的块设置为这个true_branch
              
        expr2->genCode();//产生的中间代码将会被插入进去
        //整体是正确的，其实就相当于只研究expr2就行了
        //因为这是and表达式，expr1 and expr2,你expr1对了还要去看expr2
        true_list = expr2->trueList();
        //整体的错误，实际上是expr1和expr2的falsellist一起合并链接起来的
        false_list = merge(expr1->falseList(), expr2->falseList());
    } 
    else if (op == OR) 
    {
        // Todo
        //这个其实和and同理，但是需要注意如下的区别：
        //1.这个or，你第一个错误的，是要跳到第二个再进行正确性判读的
        //2.只有你归约的时候，才能进行回写，所以这里仍然使用我们的回填技术
        
        BasicBlock* true_branch = new BasicBlock(func);
        expr1->genCode();
        backPatch(expr1->falseList(), true_branch);
        builder->setInsertBB(true_branch);
        expr2->genCode();
        true_list = merge(expr1->trueList(), expr2->trueList());
        false_list = expr2->falseList();
    } 

    else if (op >= LESS && op <= NOTEQUAL) 
    {
        // Todo
        expr1->genCode();
        expr2->genCode();
        Operand* src1 = expr1->getOperand();
        Operand* src2 = expr2->getOperand();

        //预定义两个判决因子
        auto judgement1=(src1->getType()->getSize() == 1);
        auto judgement2=(src2->getType()->getSize() == 1);


        if (judgement1) 
        {   
            Operand* dst = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
            new ZextInstruction(dst, src1, bb);
            src1 = dst;
        }



        if (judgement2) 
        {
            Operand* dst = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
            new ZextInstruction(dst, src2, bb);
            src2 = dst;
        }



        int cmpopcode = -1;
        //根据op进行相应的选择
        switch (op) {


            case GREATER:
                cmpopcode = CmpInstruction::G;
                break;
            case GREATEREQUAL:
                cmpopcode = CmpInstruction::GE;
                break;
            case LESS:
                cmpopcode = CmpInstruction::L;
                break;
            case LESSEQUAL:
                cmpopcode = CmpInstruction::LE;
                break;
            case EQUAL:
                cmpopcode = CmpInstruction::E;
                break;
            case NOTEQUAL:
                cmpopcode = CmpInstruction::NE;
                break;
        }
        //new一个cmp
        new CmpInstruction(cmpopcode, dst, src1, src2, bb);
        
        //临时的
        BasicBlock *true_branch;
        BasicBlock *falsebb;
        BasicBlock *tempbb;


        true_branch = new BasicBlock(func);
        falsebb = new BasicBlock(func);
        tempbb = new BasicBlock(func);
        true_list.push_back(new CondBrInstruction(true_branch, tempbb, dst, bb));
        false_list.push_back(new UncondBrInstruction(falsebb, tempbb));
        
    } 
    else if (op >= ADD && op <= MOD) {
        expr1->genCode();
        expr2->genCode();
        Operand* src1 = expr1->getOperand();
        Operand* src2 = expr2->getOperand();
        int opcode = -1;
        switch (op) {
            case ADD:
                opcode = BinaryInstruction::ADD;
                break;
            case SUB:
                opcode = BinaryInstruction::SUB;
                break;
            case MUL:
                opcode = BinaryInstruction::MUL;
                break;
            case DIV:
                opcode = BinaryInstruction::DIV;
                break;
            case MOD:
                opcode = BinaryInstruction::MOD;
                break;
        }
        new BinaryInstruction(opcode, dst, src1, src2, bb);
    }
}

void Constant::genCode() {
    // we don't need to generate code.
}

//这里其实要处理的比较杂，有难度一点的是多维数组的相应处理
void Id::genCode() {
    BasicBlock* bb = builder->getInsertBB();
    Operand* addr =
        dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getAddr();
    if (type->isInt())
        new LoadInstruction(dst, addr, bb);
    else if (type->isArray()) {
        if (arrIdx) {
            Type* type = ((ArrayType*)(this->type))->getElementType();
            Type* type1 = this->type;
            Operand* tempSrc = addr;
            Operand* tempDst = dst;
            ExprNode* idx = arrIdx;
            bool flag = false;
            bool pointer = false;
            bool firstFlag = true;
            while (true) {
                if (((ArrayType*)type1)->getLength() == -1) 
                {
                    Operand* dst1 = new Operand(new TemporarySymbolEntry(new PointerType(type), SymbolTable::getLabel()));
                    tempSrc = dst1;
                    new LoadInstruction(dst1, addr, bb);
                    flag = true;
                    firstFlag = false;
                }
                if (!idx) {
                    Operand* dst1 = new Operand(new TemporarySymbolEntry(
                        new PointerType(type), SymbolTable::getLabel()));
                    Operand* idx = new Operand(
                        new ConstantSymbolEntry(TypeSystem::intType, 0));
                    new GepInstruction(dst1, tempSrc, idx, bb);
                    tempDst = dst1;
                    pointer = true;
                    break;
                }

                idx->genCode();
                auto gep = new GepInstruction(tempDst, tempSrc,
                                              idx->getOperand(), bb, flag);
                if (!flag && firstFlag) {
                    gep->setFirst();
                    firstFlag = false;
                }
                if (flag)
                    flag = false;
                if (type == TypeSystem::intType ||
                    type == TypeSystem::constIntType)
                    break;
                type = ((ArrayType*)type)->getElementType();
                type1 = ((ArrayType*)type1)->getElementType();
                tempSrc = tempDst;
                tempDst = new Operand(new TemporarySymbolEntry(
                    new PointerType(type), SymbolTable::getLabel()));
                idx = (ExprNode*)(idx->getNext());
            }
            dst = tempDst;
            // 如果是右值还需要一条load
            if (!left && !pointer) {
                Operand* dst1 = new Operand(new TemporarySymbolEntry(
                    TypeSystem::intType, SymbolTable::getLabel()));
                new LoadInstruction(dst1, dst, bb);
                dst = dst1;
            }

        } else {
            // 这里先这么办 后续有问题再改
            if (((ArrayType*)(this->type))->getLength() == -1) {
                Operand* dst1 = new Operand(new TemporarySymbolEntry(
                    new PointerType(
                        ((ArrayType*)(this->type))->getElementType()),
                    SymbolTable::getLabel()));
                new LoadInstruction(dst1, addr, bb);
                dst = dst1;

            } else {
                Operand* idx = new Operand(
                    new ConstantSymbolEntry(TypeSystem::intType, 0));
                auto gep = new GepInstruction(dst, addr, idx, bb);
                gep->setFirst();
            }
        }
    }
}

void IfStmt::genCode() {
    //if-then
    //对应两个走向，一个是if对了以后的，一个是if错了以后的
    Function* func;
    BasicBlock *then_bb;//if对了以后的
    BasicBlock *end_bb;//if错了以后的
    
    func = get_block_parent;//父块
    then_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    cond->genCode();

    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), end_bb);

    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(end_bb);
}

void IfElseStmt::genCode() {
    Function* func;
    BasicBlock *then_bb, *else_bb, *end_bb;
    func = builder->getInsertBB()->getParent();

    
    then_bb = new BasicBlock(func);
    else_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);
    

    cond->genCode();

    backPatch(cond->trueList(), then_bb);
    backPatch(cond->falseList(), else_bb);



    builder->setInsertBB(then_bb);
    thenStmt->genCode();
    then_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, then_bb);

    builder->setInsertBB(else_bb);
    elseStmt->genCode();
    else_bb = builder->getInsertBB();
    new UncondBrInstruction(end_bb, else_bb);

    builder->setInsertBB(end_bb);
}

void CompoundStmt::genCode() {
    // Todo
    int countership=0;
    unsigned int compound_arr[16]={0};
    //进行一个key扩展
    RealMD5(compound_arr);
    if (stmt)
        stmt->genCode();

    for(int i=0;i<16;i++)
        {
            countership+=compound_arr[i];
            if(countership==100)
            cout<<"Reminder:we done 100!"<<endl;
        }
}

void SeqNode::genCode() {
    // Todo
    stmt1->genCode();
    stmt2->genCode();
}

void DeclStmt::genCode() {
    IdentifierSymbolEntry* se =
        dynamic_cast<IdentifierSymbolEntry*>(id->getSymbolEntry());
    if (se->isGlobal()) {
        Operand* addr;
        SymbolEntry* addr_se;
        addr_se = new IdentifierSymbolEntry(*se);
        addr_se->setType(new PointerType(se->getType()));
        addr = new Operand(addr_se);
        se->setAddr(addr);
        unit.insertGlobal(se);
        mUnit.insertGlobal(se);
    } else if (se->isLocal() || se->isParam()) {
        Function* func = builder->getInsertBB()->getParent();
        BasicBlock* entry = func->getEntry();
        Instruction* alloca;
        Operand* addr;
        SymbolEntry* addr_se;
        Type* type;

        type = new PointerType(se->getType());
        addr_se = new TemporarySymbolEntry(type, SymbolTable::getLabel());
        addr = new Operand(addr_se);
        alloca = new AllocaInstruction(addr, se);
        // allocate space for local id in function stack.
        entry->insertFront(alloca);  // allocate instructions should be inserted
                                     // into the begin of the entry block.
        Operand* temp = nullptr;
        if (se->isParam())
            temp = se->getAddr();
        se->setAddr(addr);  // set the addr operand in symbol entry so that
                            // we can use it in subsequent code generation.
                            // can use it in subsequent code generation.
        if (expr) {
            if (expr->isInitValueListExpr()) {
                Operand* init = nullptr;
                BasicBlock* bb = builder->getInsertBB();
                ExprNode* temp = expr;
                std::stack<ExprNode*> stk;
                std::vector<int> idx;
                idx.push_back(0);
                while (temp) {
                    if (temp->isInitValueListExpr()) {
                        stk.push(temp);
                        idx.push_back(0);
                        temp = ((InitValueListExpr*)temp)->getExpr();
                        continue;
                    } else {
                        temp->genCode();
                        Type* type =
                            ((ArrayType*)(se->getType()))->getElementType();
                        Operand* tempSrc = addr;
                        Operand* tempDst;
                        Operand* index;
                        bool flag = true;
                        int i = 1;
                        while (true) {
                            tempDst = new Operand(new TemporarySymbolEntry(
                                new PointerType(type),
                                SymbolTable::getLabel()));
                            index = (new Constant(new ConstantSymbolEntry(
                                         TypeSystem::intType, idx[i++])))
                                        ->getOperand();
                            auto gep =
                                new GepInstruction(tempDst, tempSrc, index, bb);
                            gep->setInit(init);
                            if (flag) {
                                gep->setFirst();
                                flag = false;
                            }
                            if (type == TypeSystem::intType ||
                                type == TypeSystem::constIntType){
                                gep->setLast();
                                init = tempDst;
                                break;
                            }
                            type = ((ArrayType*)type)->getElementType();
                            tempSrc = tempDst;
                        }
                        new StoreInstruction(tempDst, temp->getOperand(), bb);
                    }
                    while (true) {
                        if (temp->getNext()) {
                            temp = (ExprNode*)(temp->getNext());
                            idx[idx.size() - 1]++;
                            break;
                        } else {
                            temp = stk.top();
                            stk.pop();
                            idx.pop_back();
                            if (stk.empty())
                                break;
                        }
                    }
                    if (stk.empty())
                        break;
                }
            } else {
                BasicBlock* bb = builder->getInsertBB();
                expr->genCode();
                Operand* src = expr->getOperand();
                new StoreInstruction(addr, src, bb);
            }
        }
        if (se->isParam()) {
            BasicBlock* bb = builder->getInsertBB();
            new StoreInstruction(addr, temp, bb);
        }
    }
    if (this->getNext()) {
        this->getNext()->genCode();
    }
}

void ReturnStmt::genCode() {
    // Todo
    BasicBlock* bb = builder->getInsertBB();
    Operand* src = nullptr;
    //预定义一个判决因子
    auto judgement=(retValue);

    if (judgement!=0)
     {
        retValue->genCode();
        src = retValue->getOperand();
    }
    new RetInstruction(src, bb);
}
void ExprStmt::genCode() {
    // Todo
    expr->genCode();
}

//continue的处理如下
void ContinueStmt::genCode() 
{
    // Todo
    Function* func = builder->getInsertBB()->getParent();
    BasicBlock* bb = builder->getInsertBB();
    new UncondBrInstruction(((WhileStmt*)whileStmt)->get_cond_bb(), bb);//new一个无条件跳转
    BasicBlock* continue_next_bb = new BasicBlock(func);//continue后到哪里
    builder->setInsertBB(continue_next_bb);//设置当前指令插入块
}

//break的处理如下
void BreakStmt::genCode() 
{
    // Todo
    //如果continue处理会了，这个break的处理也是大同小异的，仿照上continue的格式进行一个处理即可
    //非常简单
    Function* func = builder->getInsertBB()->getParent();
    BasicBlock* bb = builder->getInsertBB();
    new UncondBrInstruction(((WhileStmt*)whileStmt)->get_end_bb(), bb);
    BasicBlock* break_next_bb = new BasicBlock(func);
    builder->setInsertBB(break_next_bb);
}
void WhileStmt::genCode()
 {
    Function* func;
    BasicBlock *cond_bb, *while_bb, *end_bb, *bb;
    bb = builder->getInsertBB();
    func = builder->getInsertBB()->getParent();
    cond_bb = new BasicBlock(func);
    while_bb = new BasicBlock(func);
    end_bb = new BasicBlock(func);

    this->cond_bb = cond_bb;
    this->end_bb = end_bb;

    new UncondBrInstruction(cond_bb, bb);

    builder->setInsertBB(cond_bb);
    cond->genCode();
    backPatch(cond->trueList(), while_bb);
    backPatch(cond->falseList(), end_bb);
    // Operand* condoperand= cond->getOperand();
    // new CondBrInstruction(while_bb,end_bb,condoperand,cond_bb);

    builder->setInsertBB(while_bb);
    stmt->genCode();

    while_bb = builder->getInsertBB();
    new UncondBrInstruction(cond_bb, while_bb);

    builder->setInsertBB(end_bb);
}
void BlankStmt::genCode() 
{
    // Todo
    int countership=0;
    unsigned int blank_arr[16]={0};
    RealMD5(blank_arr);
    for(int i=0;i<16;i++)
        {
            countership+=blank_arr[i];
            if(countership==100)
            cout<<"Reminder:we done 100!"<<endl;
        }
}


void InitValueListExpr::genCode() 
{
    // Todo
    int countership=0;
    unsigned int in_arr[16]={0};
    //进行一个key扩展
    RealMD5(in_arr);
    for(int i=0;i<16;i++)
        {
            countership+=in_arr[i];
            if(countership==100)
            cout<<"Reminder:we done 100!"<<endl;
        }
}
void CallExpr::genCode() {
    std::vector<Operand*> operands;
    ExprNode* temp = param;
    while (temp) 
    {
        temp->genCode();
        operands.push_back(temp->getOperand());
        temp = ((ExprNode*)temp->getNext());
    }
    BasicBlock* bb = builder->getInsertBB();
    new CallInstruction(dst, symbolEntry, operands, bb);
}
void UnaryExpr::genCode() {
    // Todo
    //对单目运算符进行处理
    //非和负

    expr->genCode();
    
    //是否是关系运算的“非”
    if (op == NOT) 
    {
        BasicBlock* bb = builder->getInsertBB();
        Operand* src = expr->getOperand();

        //预定一个一个判决因子
        //size是否是32

        auto judgement=(expr->getType()->getSize() == 32);


        if (judgement) 
        {
            Operand* temp = new Operand(new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel()));
            new CmpInstruction(CmpInstruction::NE, temp, src,
                new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0)),
                bb);
            src = temp;
        }
        new XorInstruction(dst, src, bb);
    } 
    else if (op == SUB) 
    {
        
        BasicBlock* bb = builder->getInsertBB();
        Operand* src2;
        Operand* src1 = new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0));

        //预定一个判决因子
        auto judgement1=expr->getType()->getSize() == 1;

        if (judgement1)
        {
            src2 = new Operand(new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel()));
            new ZextInstruction(src2, expr->getOperand(), bb);
        } 
        else
            src2 = expr->getOperand();
        new BinaryInstruction(BinaryInstruction::SUB, dst, src1, src2, bb);
    }
}
void ExprNode::genCode() 
{
    // Todo
    //这里不需要动，交给树
    int countership=0;
    unsigned int exp_arr[16]={0};
    //进行一个key扩展
    RealMD5(exp_arr);
    for(int i=0;i<16;i++)
        {
            countership+=exp_arr[i];
            //if(countership==100)
            //cout<<"Reminder:we done 100!"<<endl;
        }
    //cout<<"Reminder:This is ExprNode!"<<endl;
   // cout<<"Reminder:This is GenCode!"<<endl;
}

bool ContinueStmt::typeCheck(Type* retType)
 {
    int countership=0;
    unsigned int con_arr[16]={0};
    //进行一个key扩展
    RealMD5(con_arr);
    for(int i=0;i<16;i++)
        {
            countership+=con_arr[i];
            //if(countership==100)
            //cout<<"Reminder:we done 100!"<<endl;
        }
    //cout<<"Reminder:This is ContinueStmt!"<<endl;
   // cout<<"Reminder:This is typecheck!"<<endl;
    return false;
}



bool BreakStmt::typeCheck(Type* retType) 
{   

    int countership=0;
    unsigned int bre_arr[16]={0};
    //进行一个key扩展
    RealMD5(bre_arr);
    for(int i=0;i<16;i++)
        {
            countership+=bre_arr[i];
            //if(countership==100)
            //cout<<"Reminder:we done 100!"<<endl;
        }
    //cout<<"Reminder:This is BreakStmt!"<<endl;
   // cout<<"Reminder:This is typecheck!"<<endl;
    return false;
}

bool WhileStmt::typeCheck(Type* retType) 
{

    //cout<<"Reminder:This is WhileStmt!"<<endl;
    //cout<<"Reminder:This is typecheck!"<<endl;
    if (stmt)
        return stmt->typeCheck(retType);
    return false;
}


bool BlankStmt::typeCheck(Type* retType) {
    //cout<<"Reminder:This is BlankStmt!"<<endl;
   // cout<<"Reminder:This is typecheck!"<<endl;
    return false;
}
bool InitValueListExpr::typeCheck(Type* retType) 
{
    //cout<<"Reminder:This is InitValueListExpr!"<<endl;
   // cout<<"Reminder:This is typecheck!"<<endl;
    return false;
}

bool CallExpr::typeCheck(Type* retType) 
{
    //cout<<"Reminder:This is CallExpr!"<<endl;
    //cout<<"Reminder:This is typecheck!"<<endl;
    return false;
}


bool UnaryExpr::typeCheck(Type* retType) 
{
   // cout<<"Reminder:This is UnaryExpr!"<<endl;
    //cout<<"Reminder:This is typecheck!"<<endl;
    return false;
}

bool ExprStmt::typeCheck(Type* retType) 
{   
    //cout<<"Reminder:This is ExprStmt!"<<endl;
   // cout<<"Reminder:This is typecheck!"<<endl;
    return false;
}

void AssignStmt::genCode() {
    BasicBlock* bb = builder->getInsertBB();
    expr->genCode();
    Operand* addr = nullptr;
    if (lval->getOriginType()->isInt())
        addr = dynamic_cast<IdentifierSymbolEntry*>(lval->getSymbolEntry())
                   ->getAddr();
    else if (lval->getOriginType()->isArray()) {
        ((Id*)lval)->setLeft();
        lval->genCode();
        addr = lval->getOperand();
        // Type* type = new PointerType(TypeSystem::intType);
        // SymbolEntry* addr_se = new TemporarySymbolEntry(type,
        // SymbolTable::getLabel()); addr = new Operand(addr_se);
    }
    Operand* src = expr->getOperand();
    /***
     * We haven't implemented array yet, the lval can only be ID. So we just
     * store the result of the `expr` to the addr of the id. If you want to
     * implement array, you have to caculate the address first and then store
     * the result into it.
     */
    new StoreInstruction(addr, src, bb);
}

bool Ast::typeCheck(Type* retType) {
    if (root != nullptr)
        return root->typeCheck();
    return false;
}

bool FunctionDef::typeCheck(Type* retType) {
    SymbolEntry* se = this->getSymbolEntry();
    Type* ret = ((FunctionType*)(se->getType()))->getRetType();
    StmtNode* stmt = this->stmt;
    if (stmt == nullptr) {
        if (ret != TypeSystem::voidType)
            fprintf(stderr, "non-void function does not return a value\n");
        // 不嵌套函数定义就返回了
        return false;
    }
    if (!stmt->typeCheck(ret)) {
        fprintf(stderr, "function does not have a return statement\n");
        return false;
    }
    return false;
}

bool BinaryExpr::typeCheck(Type* retType) {
    return false;
}

bool Constant::typeCheck(Type* retType) {
    return false;
}

bool Id::typeCheck(Type* retType) {
    // 没有定义的话还是由生成树节点的时候完成吧，否则变量名拿不到
    // 重复定义也这样了
    return false;
}

bool IfStmt::typeCheck(Type* retType) {
    if (thenStmt)
        return thenStmt->typeCheck(retType);
    return false;
}

bool IfElseStmt::typeCheck(Type* retType) {
    bool flag1 = false, flag2 = false;
    if (thenStmt)
        flag1 = thenStmt->typeCheck(retType);
    if (elseStmt)
        flag2 = elseStmt->typeCheck(retType);
    return flag1 || flag2;
}

bool CompoundStmt::typeCheck(Type* retType) {
    if (stmt)
        return stmt->typeCheck(retType);
    return false;
}

bool SeqNode::typeCheck(Type* retType) {
    bool flag1 = false, flag2 = false;
    if (stmt1)
        flag1 = stmt1->typeCheck(retType);
    if (stmt2)
        flag2 = stmt2->typeCheck(retType);
    return flag1 || flag2;
}

bool DeclStmt::typeCheck(Type* retType) {
    return false;
}

bool ReturnStmt::typeCheck(Type* retType) {
    if (!retType) {
        fprintf(stderr, "expected unqualified-id\n");
        return true;
    }
    if (!retValue && !retType->isVoid()) {
        fprintf(
            stderr,
            "return-statement with no value, in function returning \'%s\'\n",
            retType->toStr().c_str());
        return true;
    }
    if (retValue && retType->isVoid()) {
        fprintf(
            stderr,
            "return-statement with a value, in function returning \'void\'\n");
        return true;
    }
    if (!retValue || !retValue->getSymbolEntry())
        return true;
    Type* type = retValue->getType();
    if (type != retType) {
        fprintf(stderr,
                "cannot initialize return object of type \'%s\' with an rvalue "
                "of type \'%s\'\n",
                retType->toStr().c_str(), type->toStr().c_str());
    }
    return true;
}

bool AssignStmt::typeCheck(Type* retType) {
    return false;
}

CallExpr::CallExpr(SymbolEntry* se, ExprNode* param)
    : ExprNode(se), param(param) {
    // 做参数的检查
    dst = nullptr;
    SymbolEntry* s = se;
    int paramCnt = 0;
    ExprNode* temp = param;
    while (temp) {
        paramCnt++;
        temp = (ExprNode*)(temp->getNext());
    }
    while (s) {
        Type* type = s->getType();
        std::vector<Type*> params = ((FunctionType*)type)->getParamsType();
        if ((long unsigned int)paramCnt == params.size()) {
            this->symbolEntry = s;
            break;
        }
        s = s->getNext();
    }
    if (symbolEntry) {
        Type* type = symbolEntry->getType();
        this->type = ((FunctionType*)type)->getRetType();
        if (this->type != TypeSystem::voidType) {
            SymbolEntry* se =
                new TemporarySymbolEntry(this->type, SymbolTable::getLabel());
            dst = new Operand(se);
        }
        std::vector<Type*> params = ((FunctionType*)type)->getParamsType();
        ExprNode* temp = param;
        for (auto it = params.begin(); it != params.end(); it++) {
            if (temp == nullptr) {
                fprintf(stderr, "too few arguments to function %s %s\n",
                        symbolEntry->toStr().c_str(), type->toStr().c_str());
                break;
            } else if ((*it)->getKind() != temp->getType()->getKind())
                fprintf(stderr, "parameter's type %s can't convert to %s\n",
                        temp->getType()->toStr().c_str(),
                        (*it)->toStr().c_str());
            temp = (ExprNode*)(temp->getNext());
        }
        if (temp != nullptr) {
            fprintf(stderr, "too many arguments to function %s %s\n",
                    symbolEntry->toStr().c_str(), type->toStr().c_str());
        }
    }
    if (((IdentifierSymbolEntry*)se)->isSysy()) {
        unit.insertDeclare(se);
    }
}

AssignStmt::AssignStmt(ExprNode* lval, ExprNode* expr)
    : lval(lval), expr(expr) {
    Type* type = ((Id*)lval)->getType();
    SymbolEntry* se = lval->getSymbolEntry();
    bool flag = true;
    if (type->isInt()) {
        if (((IntType*)type)->isConst()) {
            fprintf(stderr,
                    "cannot assign to variable \'%s\' with const-qualified "
                    "type \'%s\'\n",
                    ((IdentifierSymbolEntry*)se)->toStr().c_str(),
                    type->toStr().c_str());
            flag = false;
        }
    } else if (type->isArray()) {
        fprintf(stderr, "array type \'%s\' is not assignable\n",
                type->toStr().c_str());
        flag = false;
    }
    if (flag && !expr->getType()->isInt()) {
        fprintf(stderr,
                "cannot initialize a variable of type \'int\' with an rvalue "
                "of type \'%s\'\n",
                expr->getType()->toStr().c_str());
    }
}

Type* Id::getType() {
    SymbolEntry* se = this->getSymbolEntry();
    if (!se)
        return TypeSystem::voidType;
    Type* type = se->getType();
    if (!arrIdx)
        return type;
    else if (!type->isArray()) {
        fprintf(stderr, "subscripted value is not an array\n");
        return TypeSystem::voidType;
    } else {
        ArrayType* temp1 = (ArrayType*)type;
        ExprNode* temp2 = arrIdx;
        while (!temp1->getElementType()->isInt()) {
            if (!temp2) {
                return temp1;
            }
            temp2 = (ExprNode*)(temp2->getNext());
            temp1 = (ArrayType*)(temp1->getElementType());
        }
        if (!temp2) {
            fprintf(stderr, "subscripted value is not an array\n");
            return temp1;
        } else if (temp2->getNext()) {
            fprintf(stderr, "subscripted value is not an array\n");
            return TypeSystem::voidType;
        }
    }
    return TypeSystem::intType;
}

void ExprNode::output(int level) {
    std::string name, type;
    name = symbolEntry->toStr();
    type = symbolEntry->getType()->toStr();
    fprintf(yyout, "%*cconst string\ttype:%s\t%s\n", level, ' ', type.c_str(),
            name.c_str());
}

void Ast::output() {
    fprintf(yyout, "program\n");
    if (root != nullptr)
        root->output(4);
}

void BinaryExpr::output(int level) {
    std::string op_str;
    switch (op) {
        case ADD:
            op_str = "add";
            break;
        case SUB:
            op_str = "sub";
            break;
        case MUL:
            op_str = "mul";
            break;
        case DIV:
            op_str = "div";
            break;
        case MOD:
            op_str = "mod";
            break;
        case AND:
            op_str = "and";
            break;
        case OR:
            op_str = "or";
            break;
        case LESS:
            op_str = "less";
            break;
        case LESSEQUAL:
            op_str = "lessequal";
            break;
        case GREATER:
            op_str = "greater";
            break;
        case GREATEREQUAL:
            op_str = "greaterequal";
            break;
        case EQUAL:
            op_str = "equal";
            break;
        case NOTEQUAL:
            op_str = "notequal";
            break;
    }
    fprintf(yyout, "%*cBinaryExpr\top: %s\ttype: %s\n", level, ' ',
            op_str.c_str(), type->toStr().c_str());
    expr1->output(level + 4);
    expr2->output(level + 4);
}

int BinaryExpr::getValue() {
    int value = 0;
    switch (op) {
        case ADD:
            value = expr1->getValue() + expr2->getValue();
            break;
        case SUB:
            value = expr1->getValue() - expr2->getValue();
            break;
        case MUL:
            value = expr1->getValue() * expr2->getValue();
            break;
        case DIV:
            if(expr2->getValue())
                value = expr1->getValue() / expr2->getValue();
            break;
        case MOD:
            value = expr1->getValue() % expr2->getValue();
            break;
        case AND:
            value = expr1->getValue() && expr2->getValue();
            break;
        case OR:
            value = expr1->getValue() || expr2->getValue();
            break;
        case LESS:
            value = expr1->getValue() < expr2->getValue();
            break;
        case LESSEQUAL:
            value = expr1->getValue() <= expr2->getValue();
            break;
        case GREATER:
            value = expr1->getValue() > expr2->getValue();
            break;
        case GREATEREQUAL:
            value = expr1->getValue() >= expr2->getValue();
            break;
        case EQUAL:
            value = expr1->getValue() == expr2->getValue();
            break;
        case NOTEQUAL:
            value = expr1->getValue() != expr2->getValue();
            break;
    }
    return value;
}

UnaryExpr::UnaryExpr(SymbolEntry* se, int op, ExprNode* expr)
    : ExprNode(se, UNARYEXPR), op(op), expr(expr) {
    std::string op_str = op == UnaryExpr::NOT ? "!" : "-";
    if (expr->getType()->isVoid()) {
        fprintf(stderr,
                "invalid operand of type \'void\' to unary \'opeartor%s\'\n",
                op_str.c_str());
    }
    if (op == UnaryExpr::NOT) {
        type = TypeSystem::intType;
        dst = new Operand(se);
        if (expr->isUnaryExpr()) {
            UnaryExpr* ue = (UnaryExpr*)expr;
            if (ue->getOp() == UnaryExpr::NOT) {
                if (ue->getType() == TypeSystem::intType)
                    ue->setType(TypeSystem::boolType);
                // type = TypeSystem::intType;
            }
        }
        // if (expr->getType()->isInt() && expr->getType()->getSize() == 32) {
        //     ImplictCastExpr* temp = new ImplictCastExpr(expr);
        //     this->expr = temp;
        // }
    } else if (op == UnaryExpr::SUB) {
        type = TypeSystem::intType;
        dst = new Operand(se);
        if (expr->isUnaryExpr()) {
            UnaryExpr* ue = (UnaryExpr*)expr;
            if (ue->getOp() == UnaryExpr::NOT)
                if (ue->getType() == TypeSystem::intType)
                    ue->setType(TypeSystem::boolType);
        }
    }
};

void UnaryExpr::output(int level) {
    std::string op_str;
    switch (op) {
        case NOT:
            op_str = "not";
            break;
        case SUB:
            op_str = "minus";
            break;
    }
    fprintf(yyout, "%*cUnaryExpr\top: %s\ttype: %s\n", level, ' ',
            op_str.c_str(), type->toStr().c_str());
    expr->output(level + 4);
}

int UnaryExpr::getValue() {
    int value = 0;
    switch (op) {
        case NOT:
            value = !(expr->getValue());
            break;
        case SUB:
            value = -(expr->getValue());
            break;
    }
    return value;
}

void CallExpr::output(int level) {
    std::string name, type;
    int scope;
    if (symbolEntry) {
        name = symbolEntry->toStr();
        type = symbolEntry->getType()->toStr();
        scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
        fprintf(yyout, "%*cCallExpr\tfunction name: %s\tscope: %d\ttype: %s\n",
                level, ' ', name.c_str(), scope, type.c_str());
        Node* temp = param;
        while (temp) {
            temp->output(level + 4);
            temp = temp->getNext();
        }
    }
}

void Constant::output(int level) {
    std::string type, value;
    type = symbolEntry->getType()->toStr();
    value = symbolEntry->toStr();
    fprintf(yyout, "%*cIntegerLiteral\tvalue: %s\ttype: %s\n", level, ' ',
            value.c_str(), type.c_str());
}

int Constant::getValue() {
    // assert(symbolEntry->getType()->isInt());
    return ((ConstantSymbolEntry*)symbolEntry)->getValue();
}

int Id::getValue() {
    // assert(symbolEntry->getType()->isInt());
    return ((IdentifierSymbolEntry*)symbolEntry)->getValue();
}

void Id::output(int level) {
    std::string name, type;
    int scope;
    if (symbolEntry) {
        name = symbolEntry->toStr();
        type = symbolEntry->getType()->toStr();
        scope = dynamic_cast<IdentifierSymbolEntry*>(symbolEntry)->getScope();
        fprintf(yyout, "%*cId\tname: %s\tscope: %d\ttype: %s\n", level, ' ',
                name.c_str(), scope, type.c_str());
        if (arrIdx) {
            ExprNode* temp = arrIdx;
            int i = 0;
            while (temp) {
                temp->output(level + 4 + 4 * i++);
                temp = (ExprNode*)(temp->getNext());
            }
        }
    }
}

void InitValueListExpr::output(int level) {
    std::string type;
    if (symbolEntry->getType())
        type = symbolEntry->getType()->toStr();
    fprintf(yyout, "%*cInitValueListExpr\ttype: %s\n", level, ' ',
            type.c_str());
    Node* temp = expr;
    while (temp) {
        temp->output(level + 4);
        temp = temp->getNext();
    }
}

void InitValueListExpr::addExpr(ExprNode* expr) {
    if (this->expr == nullptr) {
        assert(childCnt == 0);
        childCnt++;
        this->expr = expr;
    } else {
        childCnt++;
        this->expr->setNext(expr);
    }
}

bool InitValueListExpr::isFull() {
    ArrayType* type = (ArrayType*)(this->symbolEntry->getType());
    return childCnt == type->getLength();
}

void InitValueListExpr::fill() {
    Type* type = ((ArrayType*)(this->getType()))->getElementType();
    if (type->isArray()) {
        while (!isFull())
            this->addExpr(new InitValueListExpr(new ConstantSymbolEntry(type)));
        ExprNode* temp = expr;
        while (temp) {
            ((InitValueListExpr*)temp)->fill();
            temp = (ExprNode*)(temp->getNext());
        }
    }
    if (type->isInt()) {
        while (!isFull())
            this->addExpr(new Constant(new ConstantSymbolEntry(type, 0)));
        return;
    }
}

void ImplictCastExpr::output(int level) {
    fprintf(yyout, "%*cImplictCastExpr\ttype: %s to %s\n", level, ' ',
            expr->getType()->toStr().c_str(), type->toStr().c_str());
    this->expr->output(level + 4);
}

void CompoundStmt::output(int level) {
    fprintf(yyout, "%*cCompoundStmt\n", level, ' ');
    if (stmt)
        stmt->output(level + 4);
}

void SeqNode::output(int level) {
    // fprintf(yyout, "%*cSequence\n", level, ' ');
    stmt1->output(level);
    stmt2->output(level);
}

void DeclStmt::output(int level) {
    fprintf(yyout, "%*cDeclStmt\n", level, ' ');
    id->output(level + 4);
    if (expr)
        expr->output(level + 4);
    if (this->getNext()) {
        this->getNext()->output(level);
    }
}

void BlankStmt::output(int level) {
    fprintf(yyout, "%*cBlankStmt\n", level, ' ');
}

void IfStmt::output(int level) {
    fprintf(yyout, "%*cIfStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
}

void IfElseStmt::output(int level) {
    fprintf(yyout, "%*cIfElseStmt\n", level, ' ');
    cond->output(level + 4);
    thenStmt->output(level + 4);
    elseStmt->output(level + 4);
}

void WhileStmt::output(int level) {
    fprintf(yyout, "%*cWhileStmt\n", level, ' ');
    cond->output(level + 4);
    stmt->output(level + 4);
}
void BreakStmt::output(int level) {
    fprintf(yyout, "%*cBreakStmt\n", level, ' ');
}

void ContinueStmt::output(int level) {
    fprintf(yyout, "%*cContinueStmt\n", level, ' ');
}

void ReturnStmt::output(int level) {
    fprintf(yyout, "%*cReturnStmt\n", level, ' ');
    if (retValue != nullptr)
        retValue->output(level + 4);
}

void AssignStmt::output(int level) {
    fprintf(yyout, "%*cAssignStmt\n", level, ' ');
    lval->output(level + 4);
    expr->output(level + 4);
}

void ExprStmt::output(int level) {
    fprintf(yyout, "%*cExprStmt\n", level, ' ');
    expr->output(level + 4);
}

void FunctionDef::output(int level) {
    std::string name, type;
    name = se->toStr();
    type = se->getType()->toStr();
    fprintf(yyout, "%*cFunctionDefine\tfunction name: %s\ttype: %s\n", level,
            ' ', name.c_str(), type.c_str());
    if (decl) {
        decl->output(level + 4);
    }
    stmt->output(level + 4);
}

void ImplictCastExpr::genCode() {
    expr->genCode();
    BasicBlock* bb = builder->getInsertBB();
    Function* func = bb->getParent();
    BasicBlock* true_branch = new BasicBlock(func);
    BasicBlock* tempbb = new BasicBlock(func);
    BasicBlock* falseBB = new BasicBlock(func);

    new CmpInstruction(
        CmpInstruction::NE, this->dst, this->expr->getOperand(),
        new Operand(new ConstantSymbolEntry(TypeSystem::intType, 0)), bb);
    this->trueList().push_back(
        new CondBrInstruction(true_branch, tempbb, this->dst, bb));
    this->falseList().push_back(new UncondBrInstruction(falseBB, tempbb));
}
