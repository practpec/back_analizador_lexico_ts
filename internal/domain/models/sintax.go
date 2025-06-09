package models

// SyntaxNode representa un nodo en el árbol sintáctico
type SyntaxNode struct {
	Type       NodeType      `json:"type"`
	Value      string        `json:"value"`
	Line       int           `json:"line"`
	Position   int           `json:"position"`
	Children   []SyntaxNode  `json:"children"`
	Attributes map[string]string `json:"attributes"`
}

// NodeType define los tipos de nodos sintácticos
type NodeType string

const (
	// Declaraciones
	PROGRAM_NODE          NodeType = "PROGRAM"
	VARIABLE_DECLARATION  NodeType = "VARIABLE_DECLARATION"
	FUNCTION_DECLARATION  NodeType = "FUNCTION_DECLARATION"
	CLASS_DECLARATION     NodeType = "CLASS_DECLARATION"
	INTERFACE_DECLARATION NodeType = "INTERFACE_DECLARATION"
	
	// Expresiones
	EXPRESSION            NodeType = "EXPRESSION"
	BINARY_EXPRESSION     NodeType = "BINARY_EXPRESSION"
	UNARY_EXPRESSION      NodeType = "UNARY_EXPRESSION"
	CALL_EXPRESSION       NodeType = "CALL_EXPRESSION"
	MEMBER_EXPRESSION     NodeType = "MEMBER_EXPRESSION"
	LITERAL_EXPRESSION    NodeType = "LITERAL_EXPRESSION"
	IDENTIFIER_EXPRESSION NodeType = "IDENTIFIER_EXPRESSION"
	
	// Sentencias
	STATEMENT             NodeType = "STATEMENT"
	BLOCK_STATEMENT       NodeType = "BLOCK_STATEMENT"
	IF_STATEMENT          NodeType = "IF_STATEMENT"
	FOR_STATEMENT         NodeType = "FOR_STATEMENT"
	WHILE_STATEMENT       NodeType = "WHILE_STATEMENT"
	RETURN_STATEMENT      NodeType = "RETURN_STATEMENT"
	EXPRESSION_STATEMENT  NodeType = "EXPRESSION_STATEMENT"
	
	// Tipos
	TYPE_ANNOTATION       NodeType = "TYPE_ANNOTATION"
	PRIMITIVE_TYPE        NodeType = "PRIMITIVE_TYPE"
	ARRAY_TYPE            NodeType = "ARRAY_TYPE"
	FUNCTION_TYPE         NodeType = "FUNCTION_TYPE"
	
	// Errores
	SYNTAX_ERROR_NODE     NodeType = "SYNTAX_ERROR"
)

// SyntaxError representa un error sintáctico
type SyntaxError struct {
	Message     string `json:"message"`
	Line        int    `json:"line"`
	Position    int    `json:"position"`
	ErrorType   string `json:"errorType"`
	Expected    string `json:"expected,omitempty"`
	Found       string `json:"found,omitempty"`
	Severity    string `json:"severity"`
}

// SemanticError representa un error semántico
type SemanticError struct {
	Message     string `json:"message"`
	Line        int    `json:"line"`
	Position    int    `json:"position"`
	ErrorType   string `json:"errorType"`
	Symbol      string `json:"symbol,omitempty"`
	SymbolType  string `json:"symbolType,omitempty"`
	Severity    string `json:"severity"`
}

// SyntaxAnalysisRequest representa la petición de análisis sintáctico
type SyntaxAnalysisRequest struct {
	Code string `json:"code"`
}

// SyntaxAnalysisResponse representa la respuesta del análisis sintáctico/semántico
type SyntaxAnalysisResponse struct {
	AST            SyntaxNode      `json:"ast"`
	SyntaxErrors   []SyntaxError   `json:"syntaxErrors"`
	SemanticErrors []SemanticError `json:"semanticErrors"`
	SymbolTable    []Symbol        `json:"symbolTable"`
	IsValid        bool            `json:"isValid"`
}

// Symbol representa un símbolo en la tabla de símbolos
type Symbol struct {
	Name       string            `json:"name"`
	Type       string            `json:"type"`
	Kind       SymbolKind        `json:"kind"`
	Line       int               `json:"line"`
	Position   int               `json:"position"`
	Scope      string            `json:"scope"`
	Used       bool              `json:"used"`
	Attributes map[string]string `json:"attributes"`
}

// SymbolKind define los tipos de símbolos
type SymbolKind string

const (
	VARIABLE_SYMBOL  SymbolKind = "VARIABLE"
	FUNCTION_SYMBOL  SymbolKind = "FUNCTION"
	CLASS_SYMBOL     SymbolKind = "CLASS"
	INTERFACE_SYMBOL SymbolKind = "INTERFACE"
	PARAMETER_SYMBOL SymbolKind = "PARAMETER"
	PROPERTY_SYMBOL  SymbolKind = "PROPERTY"
	METHOD_SYMBOL    SymbolKind = "METHOD"
)