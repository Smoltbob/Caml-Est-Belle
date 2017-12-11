import java.util.*;

interface Visitor {

    void visit(Unit e);
    void visit(Bool e);
    void visit(Int e);
    void visit(Float e);
    void visit(Not e);
    void visit(Neg e);
    void visit(Add e);
    void visit(Sub e);
    void visit(FNeg e);
    void visit(FAdd e);
    void visit(FSub e);
    void visit(FMul e);
    void visit(FDiv e);
    void visit(Eq e);
    void visit(LE e);
    void visit(If e);
    void visit(Let e);
    void visit(Var e);
    void visit(LetRec e);
    void visit(App e);
    void visit(Tuple e);
    void visit(LetTuple e);
    void visit(Array e);
    void visit(Get e);
    void visit(Put e);
}


