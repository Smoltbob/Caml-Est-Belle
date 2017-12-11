import java.util.*;

class PrintVisitor implements Visitor {
    public void visit(Unit e) {
        System.out.print("()");
    }

    public void visit(Bool e) {
        System.out.print(e.b);
    }

    public void visit(Int e) {
        System.out.print(e.i);
    }

    public void visit(Float e) {
        String s = String.format("%.2f", e.f);
        System.out.print(s);
    }

    public void visit(Not e) {
        System.out.print("(not ");
        e.e.accept(this);
        System.out.print(")");
    }

    public void visit(Neg e) {
        System.out.print("(- ");
        e.e.accept(this);
        System.out.print(")");
    }

    public void visit(Add e) {
        System.out.print("(");
        e.e1.accept(this);
        System.out.print(" + ");
        e.e2.accept(this);
        System.out.print(")");
    }

    public void visit(Sub e) {
        System.out.print("(");
        e.e1.accept(this);
        System.out.print(" - ");
        e.e2.accept(this);
        System.out.print(")");
    }

    public void visit(FNeg e){
        System.out.print("(-. ");
        e.e.accept(this);
        System.out.print(")");
    }

    public void visit(FAdd e){
        System.out.print("(");
        e.e1.accept(this);
        System.out.print(" +. ");
        e.e2.accept(this);
        System.out.print(")");
    }

    public void visit(FSub e){
        System.out.print("(");
        e.e1.accept(this);
        System.out.print(" -. ");
        e.e2.accept(this);
        System.out.print(")");
    }

    public void visit(FMul e) {
        System.out.print("(");
        e.e1.accept(this);
        System.out.print(" *. ");
        e.e2.accept(this);
        System.out.print(")");
    }

    public void visit(FDiv e){
        System.out.print("(");
        e.e1.accept(this);
        System.out.print(" /. ");
        e.e2.accept(this);
        System.out.print(")");
    }

    public void visit(Eq e){
        System.out.print("(");
        e.e1.accept(this);
        System.out.print(" = ");
        e.e2.accept(this);
        System.out.print(")");
    }

    public void visit(LE e){
        System.out.print("(");
        e.e1.accept(this);
        System.out.print(" <= ");
        e.e2.accept(this);
        System.out.print(")");
    }

    public void visit(If e){
        System.out.print("(if ");
        e.e1.accept(this);
        System.out.print(" then ");
        e.e2.accept(this);
        System.out.print(" else ");
        e.e3.accept(this);
        System.out.print(")");
    }

    public void visit(Let e) {
        System.out.print("(let ");
        System.out.print(e.id);
        System.out.print(" = ");
        e.e1.accept(this);
        System.out.print(" in ");
        e.e2.accept(this);
        System.out.print(")");
    }

    public void visit(Var e){
        System.out.print(e.id);
    }


    // print sequence of identifiers 
    static <E> void printInfix(List<E> l, String op) {
        if (l.isEmpty()) {
            return;
        }
        Iterator<E> it = l.iterator();
        System.out.print(it.next());
        while (it.hasNext()) {
            System.out.print(op + it.next());
        }
    }

    // print sequence of Exp
    void printInfix2(List<Exp> l, String op) {
        if (l.isEmpty()) {
            return;
        }
        Iterator<Exp> it = l.iterator();
        it.next().accept(this);
        while (it.hasNext()) {
            System.out.print(op);
            it.next().accept(this);
        }
    }

    public void visit(LetRec e){
        System.out.print("(let rec " + e.fd.id + " ");
        printInfix(e.fd.args, " ");
        System.out.print(" = ");
        e.fd.e.accept(this);
        System.out.print(" in ");
        e.e.accept(this);
        System.out.print(")");
    }

    public void visit(App e){
        System.out.print("(");
        e.e.accept(this);
        System.out.print(" ");
        printInfix2(e.es, " ");
        System.out.print(")");
    }

    public void visit(Tuple e){
        System.out.print("(");
        printInfix2(e.es, ", ");
        System.out.print(")");
    }

    public void visit(LetTuple e){
        System.out.print("(let (");
        printInfix(e.ids, ", ");
        System.out.print(") = ");
        e.e1.accept(this);
        System.out.print(" in ");
        e.e2.accept(this);
        System.out.print(")");
    }

    public void visit(Array e){
        System.out.print("(Array.create ");
        e.e1.accept(this);
        System.out.print(" ");
        e.e2.accept(this);
        System.out.print(")");
    }

    public void visit(Get e){
        e.e1.accept(this);
        System.out.print(".(");
        e.e2.accept(this);
        System.out.print(")");
    }

    public void visit(Put e){
        System.out.print("(");
        e.e1.accept(this);
        System.out.print(".(");
        e.e2.accept(this);
        System.out.print(") <- ");
        e.e3.accept(this);
        System.out.print(")");
    }
}


