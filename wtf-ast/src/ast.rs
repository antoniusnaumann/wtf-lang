#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Function(FunctionDeclaration),
    Record(RecordDeclaration),
    Resource(ResourceDeclaration),
    Enum(EnumDeclaration),
    Variant(VariantDeclaration),
    Import(ImportDeclaration),
    Export(ExportDeclaration),
    Package(PackageDeclaration),
    Use(UseDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<TypeAnnotation>,
    pub body: Block,
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
    pub variants: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantDeclaration {
    pub name: String,
    pub cases: Vec<VariantCase>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantCase {
    pub name: String,
    pub associated_type: Option<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportDeclaration {
    pub path: String,
    pub items: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportDeclaration {
    pub items: Vec<String>,
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
        ok_type: Box<TypeAnnotation>,
        err_type: Box<TypeAnnotation>,
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
        op: AssignmentOperator,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub mutable: bool,
    pub name: String,
    pub type_annotation: Option<TypeAnnotation>,
    pub value: Expression,
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
    },
    FieldAccess {
        object: Box<Expression>,
        field: String,
    },
    SafeFieldAccess {
        object: Box<Expression>,
        field: String,
    },
    IndexAccess {
        collection: Box<Expression>,
        index: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    None,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Arithmetic(ArithmeticOperator),
    Equal,        // '=='
    NotEqual,     // '!='
    GreaterThan,  // '>'
    LessThan,     // '<'
    GreaterEqual, // '>='
    LessEqual,    // '<='
    Contains,     // 'in'
}

impl From<ArithmeticOperator> for BinaryOperator {
    fn from(value: ArithmeticOperator) -> Self {
        BinaryOperator::Arithmetic(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArithmeticOperator {
    Add,      // '+'
    Subtract, // '-'
    Multiply, // '*'
    Divide,   // '/'
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Negate, // '-'
    Not,    // '!'
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignmentOperator {
    Assign,                   // '='
    OpAssign(BinaryOperator), // e.g. '+='
}
