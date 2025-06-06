#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub package: Option<PackageDeclaration>,
    pub uses: Vec<UseDeclaration>,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Function(FunctionDeclaration),
    Overload(OverloadDeclaration),
    Record(RecordDeclaration),
    Resource(ResourceDeclaration),
    Enum(EnumDeclaration),
    Variant(VariantDeclaration),
    Export(ExportDeclaration),
    Test(TestDeclaration),
}

impl Declaration {
    pub fn name(&self) -> &str {
        match self {
            Declaration::Function(f) => &f.name,
            Declaration::Overload(o) => &o.name,
            Declaration::Record(r) => &r.name,
            Declaration::Resource(r) => &r.name,
            Declaration::Enum(e) => &e.name,
            Declaration::Variant(v) => &v.name,
            Declaration::Export(e) => e.item.name(),
            Declaration::Test(t) => t.name(),
        }
    }
}

impl TestDeclaration {
    pub fn name(&self) -> &str {
        self.name.as_deref().unwrap_or("<test>")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<TypeAnnotation>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OverloadDeclaration {
    pub name: String,
    /// names of all functions this overload might resolve to
    pub overloads: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorDeclaration {
    pub parameters: Vec<Parameter>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordDeclaration {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResourceDeclaration {
    pub name: String,
    pub fields: Vec<Field>,
    pub constructor: Option<ConstructorDeclaration>,
    pub methods: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDeclaration {
    pub name: String,
    pub cases: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantDeclaration {
    pub name: String,
    pub cases: Vec<VariantCase>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantCase {
    pub name: String,
    pub associated_types: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExportDeclaration {
    pub item: Box<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TestDeclaration {
    pub name: Option<String>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageDeclaration {
    pub path: ModulePath,
    pub version: Option<Version>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseDeclaration {
    pub module_path: ModulePath,
    pub interface: String,
    pub types: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModulePath {
    pub owner: String,
    pub package: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Version {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    pub extras: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAnnotation {
    Simple(String),
    List(Box<TypeAnnotation>),
    Option(Box<TypeAnnotation>),
    Result {
        ok: Box<TypeAnnotation>,
        err: Box<TypeAnnotation>,
    },
    Tuple(Vec<TypeAnnotation>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: String,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    Assignment {
        target: Expression,
        value: Expression,
    },
    ExpressionStatement(Expression),
    ReturnStatement(Option<Expression>),
    BreakStatement(Option<Expression>),
    ContinueStatement,
    ThrowStatement(Expression),
    IfStatement(IfStatement),
    MatchStatement(MatchStatement),
    WhileStatement(WhileStatement),
    ForStatement(ForStatement),
    Assertion(AssertStatement),
    EmptyLine,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub mutable: bool,
    pub name: String,
    pub type_annotation: Option<TypeAnnotation>,
    pub value: Option<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Block,
    pub else_branch: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchStatement {
    pub expression: Expression,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub variable: String,
    pub iterable: Expression,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssertStatement {
    pub condition: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Identifier(String),
    Literal(Literal),
    VariantPattern {
        variant_name: String,
        sub_pattern: Option<Box<Pattern>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Identifier(String),
    BinaryExpression {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    UnaryExpression {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    YeetExpression {
        expression: Box<Expression>,
    },
    FunctionCall {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    MethodCall {
        receiver: Box<Expression>,
        method: String,
        arguments: Vec<Expression>,
        /// Whether the safe-call operator (?.) or the member access operator (.) is used
        safe: bool,
    },
    FieldAccess {
        object: Box<Expression>,
        field: String,
        /// Whether the safe-call operator (?.) or the member access operator (.) is used
        safe: bool,
    },
    IndexAccess {
        collection: Box<Expression>,
        index: Box<Expression>,
    },
    Record {
        name: Option<String>,
        members: Vec<FieldAssignment>,
    },
    ListLiteral(Vec<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAssignment {
    pub name: String,
    pub element: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    None,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Arithmetic(ArithmeticOperator),
    Logic(LogicOperator),
    Equal,        // '=='
    NotEqual,     // '!='
    GreaterThan,  // '>'
    LessThan,     // '<'
    GreaterEqual, // '>='
    LessEqual,    // '<='
    Contains,     // 'in'
    NullCoalesce, // '?'
}

impl From<ArithmeticOperator> for BinaryOperator {
    fn from(value: ArithmeticOperator) -> Self {
        BinaryOperator::Arithmetic(value)
    }
}

impl From<LogicOperator> for BinaryOperator {
    fn from(value: LogicOperator) -> Self {
        BinaryOperator::Logic(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicOperator {
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithmeticOperator {
    Add,      // '+'
    Subtract, // '-'
    Multiply, // '*'
    Divide,   // '/'
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Negate, // '-'
    Not,    // '!'
}

impl AsRef<Expression> for Expression {
    fn as_ref(&self) -> &Expression {
        self
    }
}
