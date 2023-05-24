package br.ufma.ecp;

import java.util.HashMap;
import java.util.Map;


public class SymbolTable {
    public enum Kind {
        STATIC, ARG, VAR,FIELD
    };
    
    public static record Symbol(String name, String type, Kind kind, int index){
        
    }
    
    private Map<String, Symbol> ScopeClass;
    private Map<String, Symbol> subroutineScope;
    private Map<Kind, Integer> countVars;
    
    
    public SymbolTable() {
        ScopeClass = new HashMap<>();
        subroutineScope = new HashMap<>();
        countVars = new HashMap<>();

        countVars.put(Kind.ARG, 0);
        countVars.put(Kind.VAR, 0);
        countVars.put(Kind.STATIC, 0);
        countVars.put(Kind.FIELD, 0);
        
    }
    public void startSubroutine(){
        subroutineScope.clear();
        countVars.put(Kind.VAR,0);
        countVars.put(Kind.ARG,0);
        
    }
    
    private Map<String, Symbol> scope (Kind kind){
        if(kind == Kind.FIELD || kind == Kind.STATIC){
            return ScopeClass;
        } else {
            return subroutineScope;
        }
    }
    
    void define(String name, String type, Kind kind){
        Map<String, Symbol> scopeTable = scope(kind);
        if(scopeTable.get(name) != null) throw new RuntimeException("já tem definição de variável");
        Symbol sb = new Symbol(name, type, kind,varCont(kind));
        scopeTable.put(name, sb);
        countVars.put(kind,countVars.get(kind)+1);
    }
    
    public Symbol resolve (String name){
        Symbol sb = subroutineScope.get(name);
        if (sb != null){
            return sb;
        } else {
            return ScopeClass.get(name);
        }
    }
    
    int varCont(Kind kind){
        return countVars.get(kind);
    }
    
    


}