package services

import (
	"fmt"
	"lexical-analyzer/internal/domain/models"
)

// SyntaxParser implementa el analizador sintáctico
type SyntaxParser struct {
	tokens       []models.Token
	current      int
	syntaxErrors []models.SyntaxError
}

// NewSyntaxParser crea una nueva instancia del parser sintáctico
func NewSyntaxParser(tokens []models.Token) *SyntaxParser {
	return &SyntaxParser{
		tokens:       tokens,
		current:      0,
		syntaxErrors: []models.SyntaxError{},
	}
}

// Parse realiza el análisis sintáctico y retorna el AST
func (sp *SyntaxParser) Parse() models.SyntaxNode {
	return sp.parseProgram()
}

// GetSyntaxErrors retorna los errores sintácticos encontrados
func (sp *SyntaxParser) GetSyntaxErrors() []models.SyntaxError {
	return sp.syntaxErrors
}

// parseProgram analiza el programa completo
func (sp *SyntaxParser) parseProgram() models.SyntaxNode {
	program := models.SyntaxNode{
		Type:     models.PROGRAM_NODE,
		Value:    "program",
		Line:     1,
		Position: 1,
		Children: []models.SyntaxNode{},
	}

	for !sp.isAtEnd() {
		if stmt := sp.parseStatement(); stmt.Type != "" {
			program.Children = append(program.Children, stmt)
		}
	}

	return program
}

// parseStatement analiza una sentencia
func (sp *SyntaxParser) parseStatement() models.SyntaxNode {
	// Verificar declaraciones
	if sp.match(models.RESERVED_WORD) {
		switch sp.previous().Value {
		case "let", "const", "var":
			return sp.parseVariableDeclaration()
		case "function":
			return sp.parseFunctionDeclaration()
		case "class":
			return sp.parseClassDeclaration()
		case "interface":
			return sp.parseInterfaceDeclaration()
		case "if":
			return sp.parseIfStatement()
		case "for":
			return sp.parseForStatement()
		case "while":
			return sp.parseWhileStatement()
		case "return":
			return sp.parseReturnStatement()
		}
	}

	// Si no es una declaración, intentar como expresión
	return sp.parseExpressionStatement()
}

// parseVariableDeclaration analiza declaraciones de variables
func (sp *SyntaxParser) parseVariableDeclaration() models.SyntaxNode {
	varKind := sp.previous().Value
	
	node := models.SyntaxNode{
		Type:     models.VARIABLE_DECLARATION,
		Value:    varKind,
		Line:     sp.previous().Line,
		Position: sp.previous().Position,
		Children: []models.SyntaxNode{},
		Attributes: map[string]string{
			"kind": varKind,
		},
	}

	// Esperar identificador
	if !sp.consume(models.IDENTIFIER, "Se esperaba un identificador después de '"+varKind+"'") {
		return sp.createErrorNode("Variable declaration error")
	}

	identifier := models.SyntaxNode{
		Type:     models.IDENTIFIER_EXPRESSION,
		Value:    sp.previous().Value,
		Line:     sp.previous().Line,
		Position: sp.previous().Position,
	}
	node.Children = append(node.Children, identifier)

	// Verificar tipo opcional
	if sp.match(models.DELIMITER) && sp.previous().Value == ":" {
		typeNode := sp.parseType()
		if typeNode.Type != "" {
			node.Children = append(node.Children, typeNode)
		}
	}

	// Verificar inicialización
	if sp.match(models.OPERATOR) && sp.previous().Value == "=" {
		expr := sp.parseExpression()
		if expr.Type != "" {
			node.Children = append(node.Children, expr)
		}
	}

	// Consumir punto y coma opcional
	if sp.check(models.DELIMITER) && sp.peek().Value == ";" {
		sp.advance()
	}

	return node
}

// parseFunctionDeclaration analiza declaraciones de funciones
func (sp *SyntaxParser) parseFunctionDeclaration() models.SyntaxNode {
	node := models.SyntaxNode{
		Type:     models.FUNCTION_DECLARATION,
		Value:    "function",
		Line:     sp.previous().Line,
		Position: sp.previous().Position,
		Children: []models.SyntaxNode{},
	}

	// Nombre de la función
	if !sp.consume(models.IDENTIFIER, "Se esperaba el nombre de la función") {
		return sp.createErrorNode("Function declaration error")
	}

	identifier := models.SyntaxNode{
		Type:     models.IDENTIFIER_EXPRESSION,
		Value:    sp.previous().Value,
		Line:     sp.previous().Line,
		Position: sp.previous().Position,
	}
	node.Children = append(node.Children, identifier)

	// Parámetros
	if !sp.consume(models.DELIMITER, "Se esperaba '(' después del nombre de la función") {
		return sp.createErrorNode("Function parameters error")
	}

	parameters := sp.parseParameterList()
	if parameters.Type != "" {
		node.Children = append(node.Children, parameters)
	}

	if !sp.consume(models.DELIMITER, "Se esperaba ')' después de los parámetros") {
		return sp.createErrorNode("Function parameters error")
	}

	// Tipo de retorno opcional
	if sp.match(models.DELIMITER) && sp.previous().Value == ":" {
		returnType := sp.parseType()
		if returnType.Type != "" {
			node.Children = append(node.Children, returnType)
		}
	}

	// Cuerpo de la función
	body := sp.parseBlockStatement()
	if body.Type != "" {
		node.Children = append(node.Children, body)
	}

	return node
}

// parseForStatement analiza sentencias for
func (sp *SyntaxParser) parseForStatement() models.SyntaxNode {
	node := models.SyntaxNode{
		Type:     models.FOR_STATEMENT,
		Value:    "for",
		Line:     sp.previous().Line,
		Position: sp.previous().Position,
		Children: []models.SyntaxNode{},
	}

	// Consumir paréntesis de apertura
	if !sp.consume(models.DELIMITER, "Se esperaba '(' después de 'for'") {
		return sp.createErrorNode("For statement error")
	}

	// Inicialización
	if !sp.check(models.DELIMITER) || sp.peek().Value != ";" {
		init := sp.parseStatement()
		if init.Type != "" {
			node.Children = append(node.Children, init)
		}
	}

	// Condición
	if !sp.check(models.DELIMITER) || sp.peek().Value != ";" {
		condition := sp.parseExpression()
		if condition.Type != "" {
			node.Children = append(node.Children, condition)
		}
	}

	sp.consume(models.DELIMITER, "Se esperaba ';' después de la condición")

	// Incremento
	if !sp.check(models.DELIMITER) || sp.peek().Value != ")" {
		increment := sp.parseExpression()
		if increment.Type != "" {
			node.Children = append(node.Children, increment)
		}
	}

	sp.consume(models.DELIMITER, "Se esperaba ')' después del incremento")

	// Cuerpo del loop
	body := sp.parseStatement()
	if body.Type != "" {
		node.Children = append(node.Children, body)
	}

	return node
}

// parseExpression analiza expresiones
func (sp *SyntaxParser) parseExpression() models.SyntaxNode {
	return sp.parseAssignment()
}

// parseAssignment analiza expresiones de asignación
func (sp *SyntaxParser) parseAssignment() models.SyntaxNode {
	expr := sp.parseLogicalOr()

	if sp.match(models.OPERATOR) && sp.previous().Value == "=" {
		operator := sp.previous()
		right := sp.parseAssignment()

		return models.SyntaxNode{
			Type:     models.BINARY_EXPRESSION,
			Value:    operator.Value,
			Line:     operator.Line,
			Position: operator.Position,
			Children: []models.SyntaxNode{expr, right},
		}
	}

	return expr
}

// parseLogicalOr analiza expresiones OR lógicas
func (sp *SyntaxParser) parseLogicalOr() models.SyntaxNode {
	expr := sp.parseLogicalAnd()

	for sp.match(models.OPERATOR) && sp.previous().Value == "||" {
		operator := sp.previous()
		right := sp.parseLogicalAnd()
		expr = models.SyntaxNode{
			Type:     models.BINARY_EXPRESSION,
			Value:    operator.Value,
			Line:     operator.Line,
			Position: operator.Position,
			Children: []models.SyntaxNode{expr, right},
		}
	}

	return expr
}

// parseLogicalAnd analiza expresiones AND lógicas
func (sp *SyntaxParser) parseLogicalAnd() models.SyntaxNode {
	expr := sp.parseEquality()

	for sp.match(models.OPERATOR) && sp.previous().Value == "&&" {
		operator := sp.previous()
		right := sp.parseEquality()
		expr = models.SyntaxNode{
			Type:     models.BINARY_EXPRESSION,
			Value:    operator.Value,
			Line:     operator.Line,
			Position: operator.Position,
			Children: []models.SyntaxNode{expr, right},
		}
	}

	return expr
}

// parseEquality analiza expresiones de igualdad
func (sp *SyntaxParser) parseEquality() models.SyntaxNode {
	expr := sp.parseComparison()

	for sp.match(models.OPERATOR) && (sp.previous().Value == "==" || sp.previous().Value == "!=") {
		operator := sp.previous()
		right := sp.parseComparison()
		expr = models.SyntaxNode{
			Type:     models.BINARY_EXPRESSION,
			Value:    operator.Value,
			Line:     operator.Line,
			Position: operator.Position,
			Children: []models.SyntaxNode{expr, right},
		}
	}

	return expr
}

// parseComparison analiza expresiones de comparación
func (sp *SyntaxParser) parseComparison() models.SyntaxNode {
	expr := sp.parseTerm()

	for sp.match(models.OPERATOR) && (sp.previous().Value == ">" || sp.previous().Value == ">=" || 
		sp.previous().Value == "<" || sp.previous().Value == "<=") {
		operator := sp.previous()
		right := sp.parseTerm()
		expr = models.SyntaxNode{
			Type:     models.BINARY_EXPRESSION,
			Value:    operator.Value,
			Line:     operator.Line,
			Position: operator.Position,
			Children: []models.SyntaxNode{expr, right},
		}
	}

	return expr
}

// parseTerm analiza términos (suma y resta)
func (sp *SyntaxParser) parseTerm() models.SyntaxNode {
	expr := sp.parseFactor()

	for sp.match(models.OPERATOR) && (sp.previous().Value == "+" || sp.previous().Value == "-") {
		operator := sp.previous()
		right := sp.parseFactor()
		expr = models.SyntaxNode{
			Type:     models.BINARY_EXPRESSION,
			Value:    operator.Value,
			Line:     operator.Line,
			Position: operator.Position,
			Children: []models.SyntaxNode{expr, right},
		}
	}

	return expr
}

// parseFactor analiza factores (multiplicación y división)
func (sp *SyntaxParser) parseFactor() models.SyntaxNode {
	expr := sp.parseUnary()

	for sp.match(models.OPERATOR) && (sp.previous().Value == "*" || sp.previous().Value == "/" || sp.previous().Value == "%") {
		operator := sp.previous()
		right := sp.parseUnary()
		expr = models.SyntaxNode{
			Type:     models.BINARY_EXPRESSION,
			Value:    operator.Value,
			Line:     operator.Line,
			Position: operator.Position,
			Children: []models.SyntaxNode{expr, right},
		}
	}

	return expr
}

// parseUnary analiza expresiones unarias
func (sp *SyntaxParser) parseUnary() models.SyntaxNode {
	if sp.match(models.OPERATOR) && (sp.previous().Value == "!" || sp.previous().Value == "-" || sp.previous().Value == "+") {
		operator := sp.previous()
		right := sp.parseUnary()
		return models.SyntaxNode{
			Type:     models.UNARY_EXPRESSION,
			Value:    operator.Value,
			Line:     operator.Line,
			Position: operator.Position,
			Children: []models.SyntaxNode{right},
		}
	}

	return sp.parseCall()
}

// parseCall analiza llamadas a funciones
func (sp *SyntaxParser) parseCall() models.SyntaxNode {
	expr := sp.parsePrimary()

	for {
		if sp.match(models.DELIMITER) && sp.previous().Value == "(" {
			expr = sp.finishCall(expr)
		} else if sp.match(models.DELIMITER) && sp.previous().Value == "." {
			if !sp.consume(models.IDENTIFIER, "Se esperaba el nombre de la propiedad después de '.'") {
				return sp.createErrorNode("Member access error")
			}
			property := models.SyntaxNode{
				Type:     models.IDENTIFIER_EXPRESSION,
				Value:    sp.previous().Value,
				Line:     sp.previous().Line,
				Position: sp.previous().Position,
			}
			expr = models.SyntaxNode{
				Type:     models.MEMBER_EXPRESSION,
				Value:    ".",
				Line:     expr.Line,
				Position: expr.Position,
				Children: []models.SyntaxNode{expr, property},
			}
		} else {
			break
		}
	}

	return expr
}

// parsePrimary analiza expresiones primarias
func (sp *SyntaxParser) parsePrimary() models.SyntaxNode {
	if sp.match(models.NUMBER) {
		return models.SyntaxNode{
			Type:     models.LITERAL_EXPRESSION,
			Value:    sp.previous().Value,
			Line:     sp.previous().Line,
			Position: sp.previous().Position,
			Attributes: map[string]string{
				"dataType": "number",
			},
		}
	}

	if sp.match(models.STRING) {
		return models.SyntaxNode{
			Type:     models.LITERAL_EXPRESSION,
			Value:    sp.previous().Value,
			Line:     sp.previous().Line,
			Position: sp.previous().Position,
			Attributes: map[string]string{
				"dataType": "string",
			},
		}
	}

	if sp.match(models.RESERVED_WORD) && (sp.previous().Value == "true" || sp.previous().Value == "false") {
		return models.SyntaxNode{
			Type:     models.LITERAL_EXPRESSION,
			Value:    sp.previous().Value,
			Line:     sp.previous().Line,
			Position: sp.previous().Position,
			Attributes: map[string]string{
				"dataType": "boolean",
			},
		}
	}

	if sp.match(models.IDENTIFIER) {
		return models.SyntaxNode{
			Type:     models.IDENTIFIER_EXPRESSION,
			Value:    sp.previous().Value,
			Line:     sp.previous().Line,
			Position: sp.previous().Position,
		}
	}

	if sp.match(models.DELIMITER) && sp.previous().Value == "(" {
		expr := sp.parseExpression()
		sp.consume(models.DELIMITER, "Se esperaba ')' después de la expresión")
		return expr
	}

	// Error: token inesperado
	if !sp.isAtEnd() {
		sp.addSyntaxError(fmt.Sprintf("Token inesperado: '%s'", sp.peek().Value), sp.peek().Line, sp.peek().Position)
		sp.advance() // Consumir token problemático
	}

	return models.SyntaxNode{}
}

// Métodos auxiliares

func (sp *SyntaxParser) match(tokenType models.TokenType) bool {
	if sp.check(tokenType) {
		sp.advance()
		return true
	}
	return false
}

func (sp *SyntaxParser) check(tokenType models.TokenType) bool {
	if sp.isAtEnd() {
		return false
	}
	return sp.peek().Type == tokenType
}

func (sp *SyntaxParser) advance() models.Token {
	if !sp.isAtEnd() {
		sp.current++
	}
	return sp.previous()
}

func (sp *SyntaxParser) isAtEnd() bool {
	return sp.current >= len(sp.tokens)
}

func (sp *SyntaxParser) peek() models.Token {
	if sp.isAtEnd() {
		return models.Token{Type: models.EOF, Value: "", Line: 0, Position: 0}
	}
	return sp.tokens[sp.current]
}

func (sp *SyntaxParser) previous() models.Token {
	return sp.tokens[sp.current-1]
}

func (sp *SyntaxParser) consume(tokenType models.TokenType, message string) bool {
	if sp.check(tokenType) {
		sp.advance()
		return true
	}

	currentToken := sp.peek()
	sp.addSyntaxError(message, currentToken.Line, currentToken.Position)
	return false
}

func (sp *SyntaxParser) addSyntaxError(message string, line, position int) {
	sp.syntaxErrors = append(sp.syntaxErrors, models.SyntaxError{
		Message:   message,
		Line:      line,
		Position:  position,
		ErrorType: "SYNTAX_ERROR",
		Severity:  "ERROR",
	})
}

func (sp *SyntaxParser) createErrorNode(message string) models.SyntaxNode {
	currentToken := sp.peek()
	return models.SyntaxNode{
		Type:     models.SYNTAX_ERROR_NODE,
		Value:    message,
		Line:     currentToken.Line,
		Position: currentToken.Position,
	}
}

// Métodos adicionales para completar el parser

func (sp *SyntaxParser) parseType() models.SyntaxNode {
	if sp.match(models.IDENTIFIER) {
		return models.SyntaxNode{
			Type:     models.PRIMITIVE_TYPE,
			Value:    sp.previous().Value,
			Line:     sp.previous().Line,
			Position: sp.previous().Position,
		}
	}
	return models.SyntaxNode{}
}

func (sp *SyntaxParser) parseParameterList() models.SyntaxNode {
	// Implementación simplificada
	return models.SyntaxNode{
		Type:     models.EXPRESSION,
		Value:    "parameters",
		Children: []models.SyntaxNode{},
	}
}

func (sp *SyntaxParser) parseBlockStatement() models.SyntaxNode {
	if !sp.consume(models.DELIMITER, "Se esperaba '{'") {
		return sp.createErrorNode("Block statement error")
	}

	node := models.SyntaxNode{
		Type:     models.BLOCK_STATEMENT,
		Value:    "block",
		Line:     sp.previous().Line,
		Position: sp.previous().Position,
		Children: []models.SyntaxNode{},
	}

	for !sp.check(models.DELIMITER) || sp.peek().Value != "}" {
		if sp.isAtEnd() {
			sp.addSyntaxError("Se esperaba '}' para cerrar el bloque", sp.peek().Line, sp.peek().Position)
			break
		}
		stmt := sp.parseStatement()
		if stmt.Type != "" {
			node.Children = append(node.Children, stmt)
		}
	}

	sp.consume(models.DELIMITER, "Se esperaba '}' para cerrar el bloque")
	return node
}

func (sp *SyntaxParser) parseClassDeclaration() models.SyntaxNode {
	// Implementación simplificada
	return models.SyntaxNode{
		Type:  models.CLASS_DECLARATION,
		Value: "class",
	}
}

func (sp *SyntaxParser) parseInterfaceDeclaration() models.SyntaxNode {
	// Implementación simplificada
	return models.SyntaxNode{
		Type:  models.INTERFACE_DECLARATION,
		Value: "interface",
	}
}

func (sp *SyntaxParser) parseIfStatement() models.SyntaxNode {
	// Implementación simplificada
	return models.SyntaxNode{
		Type:  models.IF_STATEMENT,
		Value: "if",
	}
}

func (sp *SyntaxParser) parseWhileStatement() models.SyntaxNode {
	// Implementación simplificada
	return models.SyntaxNode{
		Type:  models.WHILE_STATEMENT,
		Value: "while",
	}
}

func (sp *SyntaxParser) parseReturnStatement() models.SyntaxNode {
	// Implementación simplificada
	return models.SyntaxNode{
		Type:  models.RETURN_STATEMENT,
		Value: "return",
	}
}

func (sp *SyntaxParser) parseExpressionStatement() models.SyntaxNode {
	expr := sp.parseExpression()
	if expr.Type != "" {
		return models.SyntaxNode{
			Type:     models.EXPRESSION_STATEMENT,
			Value:    "expression_statement",
			Line:     expr.Line,
			Position: expr.Position,
			Children: []models.SyntaxNode{expr},
		}
	}
	return models.SyntaxNode{}
}

func (sp *SyntaxParser) finishCall(callee models.SyntaxNode) models.SyntaxNode {
	arguments := []models.SyntaxNode{}

	if !sp.check(models.DELIMITER) || sp.peek().Value != ")" {
		for {
			arg := sp.parseExpression()
			if arg.Type != "" {
				arguments = append(arguments, arg)
			}

			if !sp.match(models.DELIMITER) || sp.previous().Value != "," {
				break
			}
		}
	}

	sp.consume(models.DELIMITER, "Se esperaba ')' después de los argumentos")

	return models.SyntaxNode{
		Type:     models.CALL_EXPRESSION,
		Value:    "call",
		Line:     callee.Line,
		Position: callee.Position,
		Children: append([]models.SyntaxNode{callee}, arguments...),
	}
}