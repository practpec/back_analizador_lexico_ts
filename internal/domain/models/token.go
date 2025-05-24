package models

// Token representa un token identificado en el análisis léxico
type Token struct {
	Type     TokenType `json:"type"`
	Value    string    `json:"value"`
	Line     int       `json:"line"`
	Position int       `json:"position"`
}

// TokenType define los tipos de tokens disponibles
type TokenType string

const (
	// Palabras reservadas
	RESERVED_WORD TokenType = "RESERVED_WORD"
	
	// Identificadores
	IDENTIFIER TokenType = "IDENTIFIER"
	
	// Números
	NUMBER TokenType = "NUMBER"
	
	// Operadores
	OPERATOR TokenType = "OPERATOR"
	
	// Delimitadores
	DELIMITER TokenType = "DELIMITER"
	
	// Strings
	STRING TokenType = "STRING"
	
	// Errores
	SYNTAX_ERROR TokenType = "SYNTAX_ERROR"
	
	// Fin de archivo
	EOF TokenType = "EOF"
)

// AnalysisRequest representa la petición de análisis
type AnalysisRequest struct {
	Code string `json:"code"`
}

// AnalysisResponse representa la respuesta del análisis
type AnalysisResponse struct {
	Tokens []Token `json:"tokens"`
	Errors []Error `json:"errors"`
}

// Error representa un error encontrado durante el análisis
type Error struct {
	Message  string `json:"message"`
	Line     int    `json:"line"`
	Position int    `json:"position"`
}