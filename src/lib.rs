use std::fmt::Write;
use std::str;
use std::iter;
use std::ptr;
use std::fmt;
use std::ops;

/// abstract syntax tree for the calculator language
enum AST {
    Equals(Box<AST>, Box<AST>),
    Upper(Box<AST>, UpperOp, Box<AST>),
    Lower(Box<AST>, LowerOp, Box<AST>),
    Integer(i32, ProgIndex, IntWidth),
}

enum LowerOp {
    Mul,
    Div
}

enum UpperOp {
    Plus,
    Minus
}

/// ProgIndex is the index into the program where this Key sequence starts
type ProgIndex = usize;

/// IntWidth is the width of the integer in Key
type IntWidth = usize;


#[derive(Debug)]
pub enum CompileError {
    UnexpectedOperator(Key),
    UnexpectedNumber(Key),
    LeadingZero,
    EndOfProgram,
    DuplicateEquals,
    MissingEquals,
}

#[derive(PartialEq,Debug)]
pub enum EvalError {
    NotEquals,
    Fraction,
    DivZero,
}

/// KeyStream is used by the compiler to parse a calculator program
pub trait KeyStream {
    fn next_key(&mut self) -> Option<Key>;
    fn peek_key(&mut self) -> Option<Key>;

    fn parse_int(&mut self) -> Result<(i32, IntWidth), CompileError> {
        let mut value = match self.next_key() {
            Some(Key::Digit(v)) => v as i32,
            Some(k) => return Err(CompileError::UnexpectedOperator(k)),
            None => return Err(CompileError::EndOfProgram)
        };
        let mut width = 1;
        if value == 0 {
            return Err(CompileError::LeadingZero)
        }
        loop {
            match self.peek_key() {
                Some(Key::Digit(v)) => {
                    self.next_key();
                    width += 1;
                    value = (value * 10) + (v as i32)
                }
                _ => return Ok((value, width))
            }
        }
    }
}

/// Key is a character in the nerdle game
#[derive(Copy,Clone,Debug,PartialEq)]
pub enum Key {
    Digit(u8),
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal
}

/// PossibilityMatrix stores constraints on the board
pub struct PossibilityMatrix {
    width: usize,
    matrix: Vec<Possibility>,
}

#[derive(Copy,Clone,PartialEq,Debug)]
pub enum Possibility {
    Certain,
    Impossible,
    Unknown,
}

impl Key {

    pub fn parse(c: char) -> Result<Key, char> {
        match c {
            '+' => Ok(Key::Plus),
            '-' => Ok(Key::Minus),
            '*' => Ok(Key::Multiply),
            '/' => Ok(Key::Divide),
            '=' => Ok(Key::Equal),
            '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => 
                Ok(Key::Digit((
                    (c as i32) - ('0' as i32)) as u8
                )),
            _ => Err(c)
        }
    }

}

impl ops::Index<(usize, Key)> for PossibilityMatrix {
    type Output = Possibility;

    fn index(&self, idx: (usize, Key)) -> &Possibility {
        let (slot, key) = idx;
        assert!(slot < self.width, "slot index out of bounds");
        &self.matrix[Self::key_idx(key) + 15 * slot]
    }
}

impl ops::IndexMut<(usize, Key)> for PossibilityMatrix {
    fn index_mut(&mut self, idx: (usize, Key)) -> &mut Possibility {
        let (slot, key) = idx;
        assert!(slot < self.width, "slot index out of bounds");
        &mut self.matrix[Self::key_idx(key) + 15 * slot]
    }
}

impl PossibilityMatrix {

    const OPERATOR_KEYS: [Key; 5] = [Key::Plus, Key::Minus, Key::Multiply, Key::Divide, Key::Equal];
    const DIGIT_KEYS: [Key; 10] = [
        Key::Digit(0), Key::Digit(1), Key::Digit(2), Key::Digit(3), Key::Digit(4),
        Key::Digit(5), Key::Digit(6), Key::Digit(7), Key::Digit(8), Key::Digit(9),
    ];
    const ALL_KEYS: [Key; 15] = [
        Key::Plus, Key::Minus, Key::Multiply, Key::Divide, Key::Equal,
        Key::Digit(0), Key::Digit(1), Key::Digit(2), Key::Digit(3), Key::Digit(4),
        Key::Digit(5), Key::Digit(6), Key::Digit(7), Key::Digit(8), Key::Digit(9),
    ];

    // key_idx assigns a numeric value to the key between 0 and 14 (inclusive) for array indexing
    fn key_idx(k: Key) -> usize {
        match k {
            Key::Digit(d) => {
                assert!(d < 10, "key index {} out of bounds", d);
                d as usize
            },
            Key::Plus => 10,
            Key::Minus => 11,
            Key::Multiply => 12,
            Key::Divide => 13,
            Key::Equal => 14,
        }
    }

    pub fn blank(width: usize) -> Self {
        let num_entries = 15 * width;
        let mut matrix = Self {
            width,
            matrix: Vec::with_capacity(num_entries),
        };
        for _ in 0..num_entries {
            matrix.matrix.push(Possibility::Unknown)
        }

        // assign impossible to operators on the edges
        for key in Self::OPERATOR_KEYS {
            matrix.eliminate(0, key);
            matrix.eliminate(width - 1, key);
        }
        // also leading zero is impossible
        matrix.eliminate(0, Key::Digit(0));
        
        matrix
    }
    
    pub fn set_certain(&mut self, slot: ProgIndex, key: Key) {
        // sanity check
        assert_ne!(self[(slot, key)], Possibility::Impossible, "{:?} in slot {} was previously impossible", key, slot);

        // set the whole slot
        for eliminate_key in Self::ALL_KEYS {
            if key == eliminate_key {
                self[(slot, eliminate_key)] = Possibility::Certain
            } else {
                // If you are certain of a value, all other keys are impossible in that slot
                self.eliminate(slot, eliminate_key)
            }
        }

        if let Key::Digit(0) = key {
            // can't have an operator to the left of the 0
            for operator in Self::OPERATOR_KEYS {
                self.eliminate(slot - 1, operator)
            }
            return
        }

        if let Key::Equal = key {
            // can't have multiple equals signs
            self.eliminate_everywhere(key)
        }

        // if the key was an operator, the surrounding slots aren't possible operators either
        if Self::OPERATOR_KEYS.contains(&key) {
            for operator in Self::OPERATOR_KEYS {
                self.eliminate(slot - 1, operator);
                self.eliminate(slot + 1, operator);
            }
            // also the next digit can't be a leading 0
            self.eliminate(slot + 1, Key::Digit(0));
            return
        }
    }

    pub fn eliminate(&mut self, slot: ProgIndex, key: Key) {
        // sanity check
        assert_ne!(self[(slot, key)], Possibility::Certain, "{:?} in slot {} was previously certain", key, slot);

        self[(slot, key)] = Possibility::Impossible
    }

    pub fn eliminate_everywhere(&mut self, key: Key) {
        for slot in 0..self.width {
            // do not overwrite certain cells
            let cell = &mut self[(slot, key)];
            if *cell != Possibility::Certain {
                *cell = Possibility::Impossible
            }
        }
    }

    /// solutions calls the given callback function with each solution
    pub fn solutions<F: FnMut(String) -> bool>(&self, mut f: F) {
        let mut equation = Equation::new(self.width);
        while equation.next_equation_template(self) {
            // is it within the constraints?
            if !self.operator_check(&equation) {
                continue
            }
            // does it compile?
            let mut ast = match equation.check_compile() {
                Err(_) => continue,
                Ok(ast) => ast,
            };
            // does it make any sense?
            match ast.bounds(self) {
                Some(_) => (),
                None => continue,
            }
            // does it have any solutions?
            ast.first(self);
            while ast.next(self, None) {
                if !f(format!("{}", ast)) {
                    return
                }
            }
        }
    }

    fn can_be(&self, mut value: i32, slot: ProgIndex, width: IntWidth) -> bool {
        if value < 1 {
            return false
        }
        for i in (0..width).rev() {
            if value == 0 {
                return false
            }
            let digit = Key::Digit((value % 10) as u8);
            if let Possibility::Impossible = self[(slot + i, digit)] {
                return false 
            }
            value /= 10;
        }
        value == 0
    }

    /// first_possible_digit consumes the iterator and returns the first possible digit value 
    /// in the provided slot. If no digit is possible, returns None
    fn first_possible_digit<I: iter::Iterator<Item = i32>>(&self, slot: usize, range: I) -> Option<i32> {
        for i in range {
            match self.matrix[slot * 15 + i as usize] {
                Possibility::Impossible => continue,
                _ => return Some(i)
            }
        }
        None
    }

    /// first_int returns the smallest valid (based on constrain matrix) int greater than or equal to the provided start int
    fn first_int(&self, mut start: i32, slot: ProgIndex, width: IntWidth) -> Option<i32> {
        let mut result = 0;
        let mut base = 1;
        let mut rollover = false;
        for i in (0..width).rev() {
            let mut digit = start % 10;
            if rollover {
                digit += 1;
            }
            if i == 0 {
                digit = digit.max(1)
            }
            start /= 10;
            match self.first_possible_digit(slot + i, digit..10) {
                Some(d) => digit = d,
                None => {
                    if i == 0 {
                        return None
                    }
                    rollover = true;
                    match self.first_possible_digit(slot + i, 0..(digit + 1)) {
                        Some(d) => digit = d,
                        None => {
                            panic!("slot {} can't be a digit", slot + i);
                        }
                    }
                }
            }
            result += digit * base;
            base *= 10;
        }
        // make sure we consumed all the starting variable, otherwise it was out of bounds
        if start == 0 {
            Some(result)
        } else {
            None
        }
    }

    fn int_bounds(&self, start: ProgIndex, width: IntWidth) -> Option<(i32, i32)> {
        let mut min = 0;
        let mut max = 0;
        for slot in 0..width {
            let (min_range, max_range) = if slot == 0 {
                (1..10, (1..10).rev())
            } else {
                (0..10, (0..10).rev())
            };
            match self.first_possible_digit(start + slot, min_range) {
                Some(min_digit) => min = min * 10 + min_digit,
                None => return None,
            };
            match self.first_possible_digit(start + slot, max_range) {
                Some(max_digit) => max = max * 10 + max_digit,
                None => return None,
            };
        }
        Some((min, max))
    }

    fn any_digit(&self, slot: ProgIndex) -> Possibility {
        for key in Self::DIGIT_KEYS {
            let poss = self[(slot, key)];
            if poss != Possibility::Impossible {
                return poss
            }
        }
        Possibility::Impossible
    }

    // check if the operators in this equation can satisfy the constraint matrix
    fn operator_check(&self, e: &Equation) -> bool {
        for (slot, key) in e.keys.iter().enumerate() {
            match *key {
                Key::Digit(_) => {
                    if let Possibility::Impossible = self.any_digit(slot) {
                        return false
                    }
                },
                _ => {
                    if self[(slot, *key)] == Possibility::Impossible {
                        return false
                    }
                }
            }
        }
        return true
    }
}

/// Equation is the current game board
struct Equation {
    width: usize,
    keys: Vec<Key>,
    idx: usize,
}

impl Equation {
    
    fn new(width: usize,) -> Self {
        let mut eqn = Self {
            width,
            keys: Vec::with_capacity(width),
            idx: 0,
        };
        for _ in 0..width {
            eqn.keys.push(Key::Digit(1))
        }
        eqn
    }

    /// next_equation_template changes the underlying equation so that it compiles to something
    /// else. It is not guaranteed that the new equation compiles
    ///
    /// TODO there are some heuristics that would speed up iteration over equations and guarantee
    /// each one compiles (don't put operators next to each other, only put one = sign)
    fn next_equation_template(&mut self, constraints: &PossibilityMatrix) -> bool {
        self.idx = 0;
        for slot in (1..(self.width - 1)).rev() {
            let mut has_rolled_over = false;
            let key = &mut self.keys[slot];
            let current = match key {
                Key::Digit(_) => constraints.any_digit(slot),
                _ => constraints[(slot, *key)]
            };
            if current == Possibility::Certain {
                continue
            }
            loop {
                let possibility = match key {
                    Key::Digit(_) => {
                        *key = Key::Equal;
                        constraints[(slot, *key)]
                    },
                    Key::Equal => {
                        *key = Key::Plus;
                        constraints[(slot, *key)]
                    }
                    Key::Plus => {
                        *key = Key::Minus;
                        constraints[(slot, *key)]
                    },
                    Key::Minus => {
                        *key = Key::Multiply;
                        constraints[(slot, *key)]
                    },
                    Key::Multiply => {
                        *key = Key::Divide;
                        constraints[(slot, *key)]
                    },
                    Key::Divide => {
                        *key = Key::Digit(1);
                        if has_rolled_over {
                            panic!("I rolled over trying to find a legal equation template! {}", slot)
                        }
                        has_rolled_over = true;
                        // digits are weird, they're only possible if any digit is possible
                        constraints.any_digit(slot)
                    },
                };
                if match possibility {
                    Possibility::Certain => return true,
                    Possibility::Impossible => false,
                    Possibility::Unknown => true,
                } {
                    if has_rolled_over {
                        break
                    }
                    return true
                }
            }
        }
        false
    }

    fn check_compile(&mut self) -> Result<AST, CompileError> {
        self.idx = 0;
        for key in &mut self.keys {
            match key {
                Key::Digit(d) => *d = 1,
                _ => (),
            }
        }
        let ast = AST::compile(self)?;
        match &ast {
            AST::Equals(_, _) => Ok(ast),
            _ => Err(CompileError::MissingEquals),
        }
    }

}

impl KeyStream for Equation {
    fn next_key(&mut self) -> Option<Key> {
        let key = self.peek_key();
        self.idx += 1;
        key
    }
    fn peek_key(&mut self) -> Option<Key> {
        match self.keys.get(self.idx) {
            Some(k) => Some(*k),
            None => None,
        }
    }
}

impl KeyStream for iter::Peekable<str::Chars<'static>> {

    fn next_key(&mut self) -> Option<Key> {
        if let Some(c) = self.next() {
            match Key::parse(c) {
                Ok(k) => Some(k),
                Err(_) => panic!("cannot parse {}", c),
            }
        } else {
            None
        }
    }

    fn peek_key(&mut self) -> Option<Key> {
        if let Some(c) = self.peek() {
            match Key::parse(*c) {
                Ok(k) => Some(k),
                Err(_) => panic!("cannot parse {}", c),
            }
        } else {
            None
        }
    }
}

impl AST {

    pub fn compile<K: KeyStream>(data: &mut K) -> Result<AST, CompileError> {
        let (val, width) = data.parse_int()?;
        let mut tree = AST::Integer(val, 0, width);
        let mut index = width;
        loop {
            match data.next_key() {
                None => return Ok(tree),
                Some(k) => {
                    let (val, width) = data.parse_int()?;
                    let integer = AST::Integer(val, index + 1, width);
                    tree.append(k, integer)?;
                    index += 1 + width;
                }
            }
        }
    }

    /// bounds computes the upper and lower bound for each expression
    fn bounds(&self, constraints: &PossibilityMatrix) -> Option<(i32, i32)> {
        match self {
            AST::Equals(lhs, rhs) => {
                match (lhs.bounds(constraints), rhs.bounds(constraints)) {
                    (Some((lhs_low, lhs_high)), Some((rhs_low, rhs_high))) => {
                        if rhs_high >= lhs_low && rhs_low <= lhs_high {
                            Some((0, 0))
                        } else {
                            None
                        }
                    }
                    _ => None
                }
            }
            AST::Upper(lhs, op, rhs) => {
                match (lhs.bounds(constraints), rhs.bounds(constraints)) {
                    (Some((lhs_low, lhs_high)), Some((rhs_low, rhs_high))) => match op {
                        UpperOp::Plus => Some((lhs_low + rhs_low, lhs_high + rhs_high)),
                        UpperOp::Minus => Some((lhs_low - rhs_high, lhs_high - rhs_low)),
                    },
                    _ => None
                }
            },
            AST::Lower(lhs, op, rhs) => {
                match (lhs.bounds(constraints), rhs.bounds(constraints)) {
                    (Some((lhs_low, lhs_high)), Some((rhs_low, rhs_high))) => match op {
                        LowerOp::Mul => Some((lhs_low * rhs_low, lhs_high * rhs_high)),
                        LowerOp::Div => Some(((lhs_low + rhs_high - 1) / rhs_high, lhs_high / rhs_low)),
                    }
                    _ => None
                }
            }
            &AST::Integer(_, idx, width) => constraints.int_bounds(idx, width)
        }
    }

    
    /// promote the AST to a different enum value that contains itself. Rust will not let you do
    /// this normally because it involves some amount of time where you have moved self and later
    /// re-assigned it. If you were to panic while constructing the new value then you wouldn't
    /// complete the assignment and would be left with an empty or moved self. So we do a little
    /// unsafe to get around it. Be very careful in the closure, do not panic
    ///
    /// references:
    /// https://stackoverflow.com/a/60382120
    /// https://github.com/alecmocatta/replace_with/blob/master/src/lib.rs
    fn unsafe_replace_with<F: FnOnce(Self) -> Self>(&mut self, construct: F) {
        unsafe {
            let old_self = ptr::read(self);
            // NOTE: some terrible things may happen if you panic here
            let new_self = construct(old_self);
            ptr::write(self, new_self);
        }
    }

    fn append(&mut self, k: Key, integer: AST) -> Result<(), CompileError> {
        match k {
            Key::Digit(_) => return Err(CompileError::UnexpectedNumber(k)),
            Key::Multiply => self.append_lower(LowerOp::Mul, integer),
            Key::Divide => self.append_lower(LowerOp::Div, integer),
            Key::Plus =>  self.append_upper(UpperOp::Plus, integer),
            Key::Minus => self.append_upper(UpperOp::Minus, integer),
            Key::Equal => self.append_equals(integer)?,
        }
        Ok(())
    }

    fn append_equals(&mut self, integer: AST) -> Result<(), CompileError> {
        match self {
            AST::Equals(_, _) => Err(CompileError::DuplicateEquals),
            _ => {
                self.unsafe_replace_with(
                    |_self| AST::Equals(
                        Box::new(_self),
                        Box::new(integer),
                    )
                );
                Ok(())
            }
        }
    }

    fn append_upper(&mut self, op: UpperOp, integer: AST) {
        let side: &mut Self = match self {
            AST::Equals(_, rhs) => rhs,
            _ => self
        };
        side.unsafe_replace_with(
            |_self| AST::Upper(
                Box::new(_self),
                op,
                Box::new(integer),
            )
        );
    }

    fn append_lower(&mut self, op: LowerOp, integer: AST) {
        let side: &mut Self = match self {
            AST::Equals(_, rhs) => return rhs.append_lower(op, integer),
            AST::Upper(_, _, rhs) => rhs,
            _ => self,
        };
        side.unsafe_replace_with(
            |_self| AST::Lower(
                Box::new(_self),
                op,
                Box::new(integer)
            )
        )
    }

    fn value(&self) -> Result<i32, EvalError> {
        Ok(
            match self {
                AST::Integer(i, _, _) => *i,
                AST::Lower(lhs, op, rhs) =>
                    match op {
                        LowerOp::Mul => lhs.value()? * rhs.value()?,
                        LowerOp::Div => {
                            let rhs_value = rhs.value()?;
                            if rhs_value == 0 {
                                return Err(EvalError::DivZero)
                            }
                            let lhs_value = lhs.value()?;
                            if lhs_value % rhs_value != 0 {
                                return Err(EvalError::Fraction)
                            }
                            lhs_value / rhs_value
                        }
                    }
                AST::Upper(lhs, op, rhs) => {
                    match op {
                        UpperOp::Plus => lhs.value()? + rhs.value()?,
                        UpperOp::Minus => lhs.value()? - rhs.value()?
                    }
                }
                AST::Equals(lhs, rhs) => {
                    if lhs.value()? != rhs.value()? {
                        return Err(EvalError::NotEquals)
                    }
                    0
                }
            }
        )
    }

    fn first(&mut self, constraint: &PossibilityMatrix) {
        match self {
            AST::Integer(value, slot, width) => {
                match constraint.first_int(0, *slot, *width) {
                    Some(v) => *value = v,
                    None => panic!("AST can't be an int!")
                }
            },
            AST::Lower(lhs, _, rhs) | AST::Upper(lhs, _, rhs) | AST::Equals(lhs, rhs) => {
                lhs.first(constraint);
                rhs.first(constraint);
            },
        }
    }

    /// increment to the next valid AST integers. Returns true if successful, false otherwise
    /// the resulting AST after using this function *should* evaluate properly without any division
    /// by zero, fractions, unbalanced equations, etc...
    fn next(&mut self, constraint: &PossibilityMatrix, set: Option<i32>) -> bool {
        match self {
            AST::Equals(lhs, rhs) => Self::iter_pair_balanced(lhs, rhs, constraint, |rhs_value| (rhs_value, true)),
            AST::Upper(lhs, op, rhs) => {
                match set {
                    None => Self::iter_pair(lhs, rhs, constraint),
                    Some(set_value) => match op {
                        UpperOp::Plus => Self::iter_pair_balanced(lhs , rhs, constraint, |rhs_value| (set_value - rhs_value, true)),
                        UpperOp::Minus => Self::iter_pair_balanced(lhs , rhs, constraint, |rhs_value| (set_value + rhs_value, true)),
                    }
                }
            },
            AST::Lower(lhs, op, rhs) => {
                match set {
                    None => Self::iter_pair(lhs, rhs, constraint),
                    Some(set_value) => match op {
                        LowerOp::Mul => Self::iter_pair_balanced(lhs, rhs, constraint, |rhs_value| {
                            if rhs_value == 0 || set_value == 0 || set_value % rhs_value != 0 {
                                (0, false)
                            } else {
                                (set_value / rhs_value, true)
                            }
                        }),
                        LowerOp::Div => Self::iter_pair_balanced(lhs, rhs, constraint, |rhs_value| (rhs_value * set_value, true)),
                    }
                }
            },
            AST::Integer(v, slot, width) => match constraint.first_int(*v + 1, *slot, *width) {
                Some(value) => {
                    *v = value;
                    true
                },
                None => false
            },
        }
    }

    fn iter_pair(lhs: &mut AST, rhs: &mut AST, constraint: &PossibilityMatrix) -> bool {
        if rhs.next(constraint, None) {
            return true
        }
        rhs.first(constraint);
        lhs.next(constraint, None)
    }

    /// iter_pair_balanced increments the rhs, then tries to set lhs to a value that keeps the
    /// equation balanced as defined by the balance function (depends on the upper operation)
    fn iter_pair_balanced<F: Fn(i32) -> (i32, bool)>(lhs: &mut AST, rhs: &mut AST, constraint: &PossibilityMatrix, balance: F) -> bool {
        let lhs_is_int = match lhs {
            AST::Integer(_, _, _) => true,
            _ => false
        };

        // this is so that LHS has an opportunity to yield alternate ways to have the same value
        let mut skip_rhs_increment = true;

        loop {
            let set_value;

            // maybe increment rhs and definitely evalute it
            // might not be possible to evaluate the rhs after increment, so do it in a loop
            loop {
                if !skip_rhs_increment {
                    if !rhs.next(constraint, None) {
                        return false
                    }
                    if !lhs_is_int {
                        lhs.first(constraint);
                    }
                }
                skip_rhs_increment = false;
                let (rhs_value, okay) = match rhs.value() {
                    Ok(rhs_value) => balance(rhs_value),
                    Err(_) => continue
                };
                if okay {
                    set_value = rhs_value;
                    break
                }
            }

            // Integers cannot be set with the `Some(val)` parameter because it cannot know when to
            // roll over
            if let AST::Integer(v, slot, width) = lhs {
                if *v != set_value && constraint.can_be(set_value, *slot, *width) {
                    *v = set_value;
                    return true
                }
            } else {
                // what if there are multiple ways to satisfy LHS = set_value?
                // see the hacky `skip_rhs_increment` variable to see how we get around that
                if lhs.next(constraint, Some(set_value)) {
                    return true
                }
            }
        }
    }

}

#[derive(Debug)]
pub enum EquationError {
    Syntax(CompileError),
    Value(EvalError)
}

pub fn check_equation<K: KeyStream>(k: &mut K) -> Option<EquationError> {
    match AST::compile(k) {
        Ok(ast) => match ast {
            AST::Equals(_, _) => match ast.value() {
                Err(e) => Some(EquationError::Value(e)),
                Ok(_) => None,
            },
            _ => Some(EquationError::Syntax(CompileError::MissingEquals))
        }
        Err(e) => Some(EquationError::Syntax(e))
    }
}

impl fmt::Debug for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            AST::Equals(lhs, rhs) => write!(f, "{:?}={:?}", lhs, rhs),
            AST::Upper(lhs, op, rhs) => {
                match op {
                    UpperOp::Plus => write!(f, "({:?}+{:?})", lhs, rhs),
                    UpperOp::Minus => write!(f, "({:?}-{:?})", lhs, rhs),
                }
            },
            AST::Lower(lhs, op, rhs) => {
                match op {
                    LowerOp::Mul => write!(f, "({:?}*{:?})", lhs, rhs),
                    LowerOp::Div => write!(f, "({:?}/{:?})", lhs, rhs)
                }
            },
            AST::Integer(i, _, _) => i.fmt(f)
        }
    }
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            AST::Equals(lhs, rhs) => write!(f, "{}={}", lhs, rhs),
            AST::Upper(lhs, op, rhs) => {
                match op {
                    UpperOp::Plus => write!(f, "{}+{}", lhs, rhs),
                    UpperOp::Minus => write!(f, "{}-{}", lhs, rhs),
                }
            },
            AST::Lower(lhs, op, rhs) => {
                match op {
                    LowerOp::Mul => write!(f, "{}*{}", lhs, rhs),
                    LowerOp::Div => write!(f, "{}/{}", lhs, rhs)
                }
            },
            AST::Integer(i, _, _) => i.fmt(f)
        }
    }
}

impl fmt::Display for Key {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        f.write_char(
            match self {
                Key::Plus => '+',
                Key::Minus => '-',
                Key::Multiply => '*',
                Key::Divide => '/',
                Key::Equal => '=',
                &Key::Digit(d) => (d + '0' as u8) as char
            }
        )
    }
}

impl fmt::Display for Equation {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for k in &self.keys {
            write!(f, "{}", k)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test_compile_programs() {
        let tests = vec![
            ("1", "1", Ok(1)),
            ("10", "10", Ok(10)),
            ("2+2", "(2+2)", Ok(4)),
            ("13+2", "(13+2)", Ok(15)),
            ("2*3+4", "((2*3)+4)", Ok(10)),
            ("2+3*4", "(2+(3*4))", Ok(14)),
            ("2+3*4=84", "(2+(3*4))=84", Err(EvalError::NotEquals)),
            ("2+3*4=84-70", "(2+(3*4))=(84-70)", Ok(0)),
        ];

        for (i, test) in tests.iter().enumerate() {
            let (prog, repr, eval) = test;
            let mut stream = prog.chars().peekable();
            let ast = match AST::compile(&mut stream) {
                Err(e) => panic!("program {} `{}` failed to compile - {:?}", i, prog, e),
                Ok(ast) => ast,
            };
            
            assert_eq!(prog.to_string(), format!("{}", ast));
            assert_eq!(repr.to_string(), format!("{:?}", ast));
            assert_eq!(ast.value(), *eval);
        }
    }

    #[test]
    fn test_invalid_programs() {
        let progs = vec![
            "0000",
            "+1",
            "1+",
            "1=+",
            "-1",
            "01",
            "=",
        ];
        for prog in progs {
            let mut stream = prog.chars().peekable();
            if let Ok(_) = AST::compile(&mut stream) {
                panic!("{} should not compile", prog)
            }
        }
    }

    #[test]
    fn test_constraint_matrix() {
        let mut matrix = PossibilityMatrix::blank(8);
        assert_eq!(matrix[(0, Key::Equal)], Possibility::Impossible);
        assert_eq!(matrix[(0, Key::Digit(0))], Possibility::Impossible);
        assert_eq!(matrix[(7, Key::Equal)], Possibility::Impossible);
        assert_eq!(matrix[(7, Key::Digit(0))], Possibility::Unknown);

        matrix.set_certain(1, Key::Multiply);
        assert_eq!(matrix[(2, Key::Divide)], Possibility::Impossible);
        assert_eq!(matrix[(2, Key::Digit(1))], Possibility::Unknown);
        assert_eq!(matrix[(2, Key::Digit(0))], Possibility::Impossible);

        matrix.eliminate(2, Key::Digit(1));
        assert_eq!(matrix[(2, Key::Digit(1))], Possibility::Impossible);
        assert_eq!(matrix[(2, Key::Digit(2))], Possibility::Unknown);

        // eliminate_everywhere doesn't affect certain cells
        matrix.set_certain(7, Key::Digit(3));
        matrix.eliminate_everywhere(Key::Digit(3));
        for slot in 0..7 {
            assert_eq!(matrix[(slot, Key::Digit(3))], Possibility::Impossible);
        }
        assert_eq!(matrix[(7, Key::Digit(3))], Possibility::Certain);

        matrix.set_certain(4, Key::Digit(0));
        assert_eq!(matrix[(3, Key::Plus)], Possibility::Impossible);
        assert_eq!(matrix[(4, Key::Digit(0))], Possibility::Certain);
        assert_eq!(matrix[(4, Key::Digit(1))], Possibility::Impossible);
    }

    #[test]
    fn test_next_equation_template() {
        let mut matrix = PossibilityMatrix::blank(8);
        let mut equation = Equation::new(8);
        assert_eq!(format!("{}", equation), "11111111");

        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "111111=1");

        matrix.set_certain(6, Key::Equal);
        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "1111+1=1");
        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "1111-1=1");
        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "1111*1=1");
        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "1111/1=1");
        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "111+11=1");

        matrix.set_certain(4, Key::Digit(3));
        equation.next_equation_template(&matrix);
        assert_eq!(format!("{}", equation), "111-11=1");
    }

    #[test]
    fn test_ast_bounds() {
        let tests = vec![
            ("1", Some((1, 7)), {
                let mut c = PossibilityMatrix::blank(1);
                c.eliminate_everywhere(Key::Digit(9));
                c.eliminate_everywhere(Key::Digit(8));
                c
            }),
            ("10", Some((10, 80)), {
                let mut c = PossibilityMatrix::blank(2);
                c.eliminate(0, Key::Digit(9));
                c.set_certain(1, Key::Digit(0));
                c
            }),
            ("1+1", Some((2, 15)), {
                let mut c = PossibilityMatrix::blank(3);
                c.eliminate_everywhere(Key::Digit(9));
                c.eliminate(0, Key::Digit(8));
                c
            }),
            ("1*1", Some((1, 81)), {
                let mut c = PossibilityMatrix::blank(3);
                c.eliminate_everywhere(Key::Digit(6));
                c.eliminate(0, Key::Digit(8));
                c
            }),
            ("10/1", Some((3, 29)), {
                let mut c = PossibilityMatrix::blank(4);
                c.set_certain(0, Key::Digit(2));
                c
            }),
            ("11+1*1", Some((13, 152)), {
                let mut c = PossibilityMatrix::blank(6);
                c.eliminate_everywhere(Key::Digit(9));
                c.eliminate(1, Key::Digit(0));
                c.eliminate(1, Key::Digit(1));
                c
            }),
            ("11+1*1/1", Some((11, 180)), PossibilityMatrix::blank(8)),
            ("11=1", None, PossibilityMatrix::blank(4)),
            ("1=1", Some((0, 0)), PossibilityMatrix::blank(3)),
            ("1=1", None, {
                let mut c = PossibilityMatrix::blank(3);
                c.set_certain(0, Key::Digit(1));
                c.set_certain(2, Key::Digit(2));
                c
            }),
        ];

        for (e, bounds, constraint) in tests {
            let ast = AST::compile(&mut e.chars().peekable()).unwrap();
            assert_eq!(ast.bounds(&constraint), bounds, "{}", e);
        }
    }

    #[test]
    fn test_first_int() {
        let mut matrix = PossibilityMatrix::blank(4);
        matrix.set_certain(0, Key::Digit(3));
        matrix.eliminate(1, Key::Digit(7));
        assert_eq!(matrix.first_int(0, 0, 2), Some(30));
        assert_eq!(matrix.first_int(30, 0, 2), Some(30));
        assert_eq!(matrix.first_int(37, 0, 2), Some(38));
        assert_eq!(matrix.first_int(40, 0, 2), None);

        matrix.eliminate(3, Key::Digit(9));
        matrix.eliminate(3, Key::Digit(0));
        matrix.eliminate(2, Key::Digit(2));
        assert_eq!(matrix.first_int(19, 2, 2), Some(31));
    }

    #[test]
    fn test_iter_pair_balanced() {
        let mut matrix = PossibilityMatrix::blank(5);
        let mut prog = "1*1+1".chars().peekable();
        let mut ast = AST::compile(&mut prog).unwrap();
        
        ast.first(&matrix);
        assert_eq!(format!("{}", ast), "1*1+1");
        ast.next(&matrix, None);
        assert_eq!(format!("{}", ast), "1*1+2");
        matrix.set_certain(4, Key::Digit(4));
        ast.next(&matrix, None);
        assert_eq!(format!("{}", ast), "1*1+4");
        
        let mut answers = vec!["6*2+4", "4*3+4", "3*4+4", "2*6+4"];
        for answer in answers {
            assert!(ast.next(&matrix, Some(16)), "{}", ast);
            assert_eq!(format!("{}", ast), answer);
        }

        matrix = PossibilityMatrix::blank(5);
        ast.first(&matrix);
        answers = vec![
            "5*3+1", "3*5+1", "7*2+2", "2*7+2", "6*2+4", 
            "4*3+4", "3*4+4", "2*6+4", "5*2+6", "2*5+6", 
            "9*1+7", "3*3+7", "1*9+7", "8*1+8", "4*2+8",
            "2*4+8", "1*8+8", "7*1+9", "1*7+9",
        ];
        for answer in answers {
            println!("testing {}", answer);
            assert!(ast.next(&matrix, Some(16)), "{}", ast);
            assert_eq!(format!("{}", ast), answer);
        }
        assert!(!ast.next(&matrix, Some(16)), "{}", ast);

        // run the test again, with some new constraints
        matrix.eliminate(0, Key::Digit(3));
        matrix.eliminate(4, Key::Digit(6));
        matrix.eliminate_everywhere(Key::Digit(8));
        ast.first(&matrix);
        answers = vec![
            "5*3+1", "7*2+2", "2*7+2", "6*2+4", "4*3+4", 
            "2*6+4", "9*1+7", "1*9+7", "7*1+9", "1*7+9",
        ];
        for answer in answers {
            println!("testing {}", answer);
            assert!(ast.next(&matrix, Some(16)), "{}", ast);
            assert_eq!(format!("{}", ast), answer);
        }
        assert!(!ast.next(&matrix, Some(16)), "{}", ast);
    }

    #[test]
    fn test_full_algorithm() {
        // this is the nerdle I'm looking at right now, I can't solve it :/
        let mut constraints = PossibilityMatrix::blank(8);
        constraints.eliminate_everywhere(Key::Digit(1));
        constraints.eliminate_everywhere(Key::Digit(2));
        constraints.eliminate_everywhere(Key::Digit(5));
        constraints.eliminate_everywhere(Key::Plus);
        constraints.eliminate_everywhere(Key::Multiply);
        constraints.eliminate(0, Key::Digit(9));
        constraints.eliminate(1, Key::Digit(3));
        constraints.eliminate(3, Key::Digit(3));
        constraints.eliminate(3, Key::Digit(3));
        constraints.eliminate(4, Key::Equal);
        constraints.eliminate(5, Key::Equal);
        constraints.eliminate(5, Key::Digit(7));
        constraints.eliminate(6, Key::Digit(4));
        constraints.eliminate(7, Key::Digit(4));
        constraints.eliminate(7, Key::Digit(7));

        // picked one of the outputs, now I know this
        constraints.eliminate(0, Key::Digit(3));
        constraints.eliminate(1, Key::Digit(8));
        constraints.set_certain(2, Key::Digit(7));
        constraints.eliminate(3, Key::Equal);
        constraints.eliminate(4, Key::Digit(4));
        constraints.set_certain(5, Key::Digit(3));
        // this constraint was wrong in hindsight
        constraints.set_certain(7, Key::Digit(9));

        let mut found = false;
        constraints.solutions(|s| {
            assert!(!found, "only 1 solution should exist");
            assert_eq!(s, "747/83=9");
            found = true;
            true
        });
        assert!(found, "didn't find the answer")
    }

}
