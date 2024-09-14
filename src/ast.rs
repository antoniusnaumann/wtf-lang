#[derive(Debug, Clone)]
pub struct Module {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<TypeAnnotation>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct ConstructorDeclaration {
    pub parameters: Vec<Parameter>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct RecordDeclaration {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct ResourceDeclaration {
    pub name: String,
    pub fields: Vec<Field>,
    pub constructor: Option<ConstructorDeclaration>,
    pub methods: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub struct EnumDeclaration {
    pub name: String,
    pub variants: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct VariantDeclaration {
    pub name: String,
    pub cases: Vec<VariantCase>,
}

#[derive(Debug, Clone)]
pub struct VariantCase {
    pub name: String,
    pub associated_type: Option<TypeAnnotation>,
}

#[derive(Debug, Clone)]
pub struct ImportDeclaration {
    pub path: String,
    pub items: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct ExportDeclaration {
    pub items: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct PackageDeclaration {
    pub name: String,
    pub version: String,
}

#[derive(Debug, Clone)]
pub struct UseDeclaration {
    pub module_path: String,
    pub items: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, Clone)]
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
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub mutable: bool,
    pub name: String,
    pub type_annotation: Option<TypeAnnotation>,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Block,
    pub else_branch: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct MatchStatement {
    pub expression: Expression,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub variable: String,
    pub iterable: Expression,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Identifier(String),
    Literal(Literal),
    VariantPattern {
        variant_name: String,
        sub_pattern: Option<Box<Pattern>>,
    },
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,          // '+'
    Subtract,     // '-'
    Multiply,     // '*'
    Divide,       // '/'
    Equal,        // '=='
    NotEqual,     // '!='
    GreaterThan,  // '>'
    LessThan,     // '<'
    GreaterEqual, // '>='
    LessEqual,    // '<='
    Concat,       // '++'
    Contains,     // 'in'
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Negate, // '-'
    Not,    // '!'
}
