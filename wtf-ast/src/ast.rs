use wtf_tokens::Span;

pub trait Node {
    fn span(&self) -> Span;
}

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

impl Node for Declaration {
    fn span(&self) -> Span {
        match self {
            Declaration::Function(inner) => inner.span,
            Declaration::Overload(inner) => inner.span,
            Declaration::Record(inner) => inner.span,
            Declaration::Resource(inner) => inner.span,
            Declaration::Enum(inner) => inner.span,
            Declaration::Variant(inner) => inner.span,
            Declaration::Export(inner) => inner.span,
            Declaration::Test(inner) => inner.span,
        }
    }
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
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OverloadDeclaration {
    pub name: String,
    /// names of all functions this overload might resolve to
    pub overloads: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstructorDeclaration {
    pub parameters: Vec<Parameter>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordDeclaration {
    pub name: String,
    pub fields: Vec<Field>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ResourceDeclaration {
    pub name: String,
    pub fields: Vec<Field>,
    pub constructor: Option<ConstructorDeclaration>,
    pub methods: Vec<FunctionDeclaration>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDeclaration {
    pub name: String,
    pub cases: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantDeclaration {
    pub name: String,
    pub cases: Vec<VariantCase>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantCase {
    pub name: String,
    pub associated_types: Vec<Field>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExportDeclaration {
    pub item: Box<Declaration>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TestDeclaration {
    pub name: Option<String>,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackageDeclaration {
    pub path: ModulePath,
    pub version: Option<Version>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseDeclaration {
    pub module_path: ModulePath,
    pub interface: String,
    pub types: Vec<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModulePath {
    pub owner: String,
    pub package: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Version {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    pub extras: Option<String>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub name: String,
    pub type_annotation: TypeAnnotation,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeAnnotation {
    pub kind: TypeAnnotationKind,
    pub span: Span,
}

impl TypeAnnotation {
    pub fn simple(name: String, span: Span) -> TypeAnnotation {
        Self {
            kind: TypeAnnotationKind::Simple(name),
            span,
        }
    }

    pub fn list(inner: TypeAnnotation, span: Span) -> TypeAnnotation {
        Self {
            kind: TypeAnnotationKind::List(inner.into()),
            span,
        }
    }

    pub fn option(inner: TypeAnnotation, span: Span) -> TypeAnnotation {
        Self {
            kind: TypeAnnotationKind::Option(inner.into()),
            span,
        }
    }

    pub fn result(ok: TypeAnnotation, err: TypeAnnotation, span: Span) -> TypeAnnotation {
        Self {
            kind: TypeAnnotationKind::Result {
                ok: ok.into(),
                err: err.into(),
            },
            span,
        }
    }

    pub fn tuple(items: Vec<TypeAnnotation>, span: Span) -> TypeAnnotation {
        Self {
            kind: TypeAnnotationKind::Tuple(items),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeAnnotationKind {
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
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub name: String,
    pub type_annotation: TypeAnnotation,
    pub span: Span,
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
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Block,
    pub else_branch: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchStatement {
    pub expression: Expression,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub variable: String,
    pub iterable: Expression,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssertStatement {
    pub condition: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Identifier(String),
    Literal(Literal),
    VariantPattern {
        variant_name: String,
        sub_pattern: Option<Box<Pattern>>,
    },
}

impl PatternKind {
    pub fn spanned(self, span: Span) -> Pattern {
        Pattern { kind: self, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

impl Node for [Expression] {
    fn span(&self) -> Span {
        let mut span = Span { start: 0, end: 0 };
        for expr in self {
            span = expr.span;
        }
        span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
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

impl ExpressionKind {
    pub fn spanned(self, span: Span) -> Expression {
        Expression { kind: self, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAssignment {
    pub name: String,
    pub element: Expression,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    None,
}

impl LiteralKind {
    pub fn spanned(self, span: Span) -> Literal {
        Literal { kind: self, span }
    }
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
