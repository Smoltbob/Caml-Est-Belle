abstract class Type {
    private static int x = 0;
    static Type gen() {
        return new TVar("?" + x++);
    }
    
}

class TUnit extends Type { }

class TBool extends Type { }

class TInt extends Type { }

class TFloat extends Type { }

class TFun extends Type { }

class TTuple extends Type { }

class TArray extends Type { }

class TVar extends Type {
    String v;
    TVar(String v) {
        this.v = v;
    }
    @Override
    public String toString() {
        return v; 
    }
}

