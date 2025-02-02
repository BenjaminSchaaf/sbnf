#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TextLocation {
    pub line: u32,
    pub column: u32,
}

impl TextLocation {
    pub const INITIAL: TextLocation = TextLocation { line: 0, column: 0 };

    pub fn new(line: u32, column: u32) -> TextLocation {
        TextLocation { line, column }
    }

    pub fn from_tuple((line, column): (u32, u32)) -> TextLocation {
        TextLocation::new(line, column)
    }

    pub fn invalid() -> TextLocation {
        TextLocation::new(u32::MAX, u32::MAX)
    }

    pub fn increment(&mut self, chr: char) {
        if chr == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }

    pub fn with_source<'a>(
        &'a self,
        source: &'a str,
    ) -> TextLocationWithSource<'a> {
        TextLocationWithSource { loc: *self, source }
    }
}

impl std::fmt::Display for TextLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.column + 1)
    }
}

impl std::fmt::Debug for TextLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}:{:?}", self.line, self.column)
    }
}

pub struct TextLocationWithSource<'a> {
    loc: TextLocation,
    source: &'a str,
}

impl std::fmt::Display for TextLocationWithSource<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display_line = self.loc.line + 1;

        // TODO: There's probably a fast way to do this...
        let source_line =
            self.source.split('\n').nth(self.loc.line as usize).unwrap();

        writeln!(f, "{} | {}", display_line, source_line)?;

        let line_log10 = ((display_line as f64).log2() / 10f64.log2()) as u32;
        for _ in 0..line_log10 + 1 {
            write!(f, " ")?;
        }

        write!(f, "   ")?;
        for _ in 0..self.loc.column {
            write!(f, " ")?;
        }
        write!(f, "^")
    }
}
