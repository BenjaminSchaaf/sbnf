use bumpalo::Bump;
use hashbrown::HashMap;
use std::cell::RefCell;
use std::num::NonZeroU32;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(NonZeroU32);

pub struct SymbolTableImpl {
    table: HashMap<&'static str, Symbol>,
    symbols: Vec<&'static str>,
    allocator: Bump,
}

pub struct SymbolTable(RefCell<SymbolTableImpl>);

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable(RefCell::new(SymbolTableImpl {
            table: HashMap::new(),
            symbols: vec![""],
            allocator: Bump::new(),
        }))
    }

    pub fn intern(&self, s: &str) -> Symbol {
        let mut this = self.0.borrow_mut();

        if let Some(sym) = this.table.get(s) {
            return *sym;
        }

        let s = this.allocator.alloc_str(s);
        let s: &'static str = unsafe { std::mem::transmute(s) };

        debug_assert!(!this.symbols.is_empty());
        debug_assert!(this.symbols.len() < u32::MAX as usize);
        let sym = Symbol(unsafe {
            NonZeroU32::new_unchecked(this.symbols.len() as u32)
        });
        this.table.insert(s, sym);
        this.symbols.push(s);
        sym
    }

    pub fn resolve(&self, symbol: Symbol) -> &str {
        let this = self.0.borrow();

        debug_assert!((symbol.0.get() as usize) < this.symbols.len());
        this.symbols[symbol.0.get() as usize]
    }
}
