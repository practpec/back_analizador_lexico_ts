package services

import (
	"fmt"
	"lexical-analyzer/internal/domain/models"
	"strings"
)

// SemanticAnalyzer implementa el analizador semántico
type SemanticAnalyzer struct {
	ast             models.SyntaxNode
	symbolTable     map[string]models.Symbol
	scopes          []string
	currentScope    string
	semanticErrors  []models.SemanticError
	usedSymbols     map[string]bool
}

// NewSemanticAnalyzer crea una nueva instancia del analizador semántico
func NewSemanticAnalyzer(ast models.SyntaxNode) *SemanticAnalyzer {
	return &SemanticAnalyzer{
		ast:            ast,
		symbolTable:    make(map[string]models.Symbol),
		scopes:         []string{"global"},
		currentScope:   "global",
		semanticErrors: []models.SemanticError{},
		usedSymbols:    make(map[string]bool),
	}
}

// Analyze realiza el análisis semántico completo
func (sa *SemanticAnalyzer) Analyze() ([]models.Symbol, []models.SemanticError) {
	// Primera pasada: construir tabla de símbolos
	sa.buildSymbolTable(sa.ast)
	
	// Segunda pasada: verificar uso y tipos
	sa.checkSemantics(sa.ast)
	
	// Verificar símbolos no utilizados
	sa.checkUnusedSymbols()
	
	// Convertir tabla de símbolos a slice
	symbols := make([]models.Symbol, 0, len(sa.symbolTable))
	for _, symbol := range sa.symbolTable {
		symbols = append(symbols, symbol)
	}
	
	return symbols, sa.semanticErrors
}

// buildSymbolTable construye la tabla de símbolos
func (sa *SemanticAnalyzer) buildSymbolTable(node models.SyntaxNode) {
	switch node.Type {
	case models.VARIABLE_DECLARATION:
		sa.handleVariableDeclaration(node)
	case models.FUNCTION_DECLARATION:
		sa.handleFunctionDeclaration(node)
	case models.CLASS_DECLARATION:
		sa.handleClassDeclaration(node)
	case models.INTERFACE_DECLARATION:
		sa.handleInterfaceDeclaration(node)
	case models.BLOCK_STATEMENT:
		sa.enterScope(fmt.Sprintf("block_%d_%d", node.Line, node.Position))
		for _, child := range node.Children {
			sa.buildSymbolTable(child)
		}
		sa.exitScope()
	default:
		for _, child := range node.Children {
			sa.buildSymbolTable(child)
		}
	}
}

// checkSemantics verifica la semántica del código
func (sa *SemanticAnalyzer) checkSemantics(node models.SyntaxNode) {
	switch node.Type {
	case models.IDENTIFIER_EXPRESSION:
		sa.checkIdentifierUsage(node)
	case models.BINARY_EXPRESSION:
		sa.checkBinaryExpression(node)
	case models.CALL_EXPRESSION:
		sa.checkFunctionCall(node)
	}
	
	// Recursivamente verificar hijos
	for _, child := range node.Children {
		sa.checkSemantics(child)
	}
}

// handleVariableDeclaration procesa declaraciones de variables
func (sa *SemanticAnalyzer) handleVariableDeclaration(node models.SyntaxNode) {
	if len(node.Children) == 0 {
		return
	}
	
	identifier := node.Children[0]
	if identifier.Type != models.IDENTIFIER_EXPRESSION {
		return
	}
	
	varName := identifier.Value
	symbolKey := sa.getSymbolKey(varName)
	
	// Verificar si ya existe en el scope actual
	if _, exists := sa.symbolTable[symbolKey]; exists {
		sa.addSemanticError(
			fmt.Sprintf("Variable '%s' ya está declarada en este scope", varName),
			identifier.Line,
			identifier.Position,
			"REDECLARATION",
			varName,
			"VARIABLE",
		)
		return
	}
	
	// Determinar tipo
	varType := "any"
	if len(node.Children) > 1 && node.Children[1].Type == models.TYPE_ANNOTATION {
		varType = node.Children[1].Value
	} else if len(node.Children) > 2 {
		// Inferir tipo de la inicialización
		varType = sa.inferType(node.Children[2])
	}
	
	// Verificar si const tiene inicialización
	if node.Attributes["kind"] == "const" && len(node.Children) < 3 {
		sa.addSemanticError(
			fmt.Sprintf("Constante '%s' debe ser inicializada", varName),
			identifier.Line,
			identifier.Position,
			"MISSING_INITIALIZER",
			varName,
			"CONST",
		)
	}
	
	// Agregar a tabla de símbolos
	symbol := models.Symbol{
		Name:     varName,
		Type:     varType,
		Kind:     models.VARIABLE_SYMBOL,
		Line:     identifier.Line,
		Position: identifier.Position,
		Scope:    sa.currentScope,
		Used:     false,
		Attributes: map[string]string{
			"kind": node.Attributes["kind"],
		},
	}
	
	sa.symbolTable[symbolKey] = symbol
}

// handleFunctionDeclaration procesa declaraciones de funciones
func (sa *SemanticAnalyzer) handleFunctionDeclaration(node models.SyntaxNode) {
	if len(node.Children) == 0 {
		return
	}
	
	identifier := node.Children[0]
	if identifier.Type != models.IDENTIFIER_EXPRESSION {
		return
	}
	
	funcName := identifier.Value
	symbolKey := sa.getSymbolKey(funcName)
	
	// Verificar si ya existe
	if _, exists := sa.symbolTable[symbolKey]; exists {
		sa.addSemanticError(
			fmt.Sprintf("Función '%s' ya está declarada", funcName),
			identifier.Line,
			identifier.Position,
			"REDECLARATION",
			funcName,
			"FUNCTION",
		)
		return
	}
	
	// Determinar tipo de retorno
	returnType := "void"
	for _, child := range node.Children {
		if child.Type == models.TYPE_ANNOTATION {
			returnType = child.Value
			break
		}
	}
	
	// Agregar función a tabla de símbolos
	symbol := models.Symbol{
		Name:     funcName,
		Type:     returnType,
		Kind:     models.FUNCTION_SYMBOL,
		Line:     identifier.Line,
		Position: identifier.Position,
		Scope:    sa.currentScope,
		Used:     false,
	}
	
	sa.symbolTable[symbolKey] = symbol
	
	// Entrar al scope de la función
	sa.enterScope(fmt.Sprintf("function_%s", funcName))
	
	// Procesar parámetros y cuerpo
	for _, child := range node.Children {
		sa.buildSymbolTable(child)
	}
	
	sa.exitScope()
}

// handleClassDeclaration procesa declaraciones de clases
func (sa *SemanticAnalyzer) handleClassDeclaration(node models.SyntaxNode) {
	if len(node.Children) == 0 {
		return
	}
	
	identifier := node.Children[0]
	className := identifier.Value
	symbolKey := sa.getSymbolKey(className)
	
	symbol := models.Symbol{
		Name:     className,
		Type:     "class",
		Kind:     models.CLASS_SYMBOL,
		Line:     identifier.Line,
		Position: identifier.Position,
		Scope:    sa.currentScope,
		Used:     false,
	}
	
	sa.symbolTable[symbolKey] = symbol
	
	// Entrar al scope de la clase
	sa.enterScope(fmt.Sprintf("class_%s", className))
	
	for _, child := range node.Children[1:] {
		sa.buildSymbolTable(child)
	}
	
	sa.exitScope()
}

// handleInterfaceDeclaration procesa declaraciones de interfaces
func (sa *SemanticAnalyzer) handleInterfaceDeclaration(node models.SyntaxNode) {
	if len(node.Children) == 0 {
		return
	}
	
	identifier := node.Children[0]
	interfaceName := identifier.Value
	symbolKey := sa.getSymbolKey(interfaceName)
	
	symbol := models.Symbol{
		Name:     interfaceName,
		Type:     "interface",
		Kind:     models.INTERFACE_SYMBOL,
		Line:     identifier.Line,
		Position: identifier.Position,
		Scope:    sa.currentScope,
		Used:     false,
	}
	
	sa.symbolTable[symbolKey] = symbol
}

// checkIdentifierUsage verifica el uso de identificadores
func (sa *SemanticAnalyzer) checkIdentifierUsage(node models.SyntaxNode) {
	varName := node.Value
	symbol := sa.findSymbol(varName)
	
	if symbol == nil {
		sa.addSemanticError(
			fmt.Sprintf("Variable '%s' no está declarada", varName),
			node.Line,
			node.Position,
			"UNDEFINED_VARIABLE",
			varName,
			"IDENTIFIER",
		)
		return
	}
	
	// Marcar como usado
	symbolKey := sa.getSymbolKeyInScope(varName, symbol.Scope)
	if sym, exists := sa.symbolTable[symbolKey]; exists {
		sym.Used = true
		sa.symbolTable[symbolKey] = sym
	}
	
	sa.usedSymbols[symbolKey] = true
}

// checkBinaryExpression verifica expresiones binarias
func (sa *SemanticAnalyzer) checkBinaryExpression(node models.SyntaxNode) {
	if len(node.Children) != 2 {
		return
	}
	
	leftType := sa.getExpressionType(node.Children[0])
	rightType := sa.getExpressionType(node.Children[1])
	operator := node.Value
	
	// Verificar compatibilidad de tipos
	if !sa.areTypesCompatible(leftType, rightType, operator) {
		sa.addSemanticError(
			fmt.Sprintf("Operador '%s' no puede ser aplicado a tipos '%s' y '%s'", 
				operator, leftType, rightType),
			node.Line,
			node.Position,
			"TYPE_MISMATCH",
			operator,
			"OPERATOR",
		)
	}
}

// checkFunctionCall verifica llamadas a funciones
func (sa *SemanticAnalyzer) checkFunctionCall(node models.SyntaxNode) {
	if len(node.Children) == 0 {
		return
	}
	
	callee := node.Children[0]
	if callee.Type != models.IDENTIFIER_EXPRESSION {
		return
	}
	
	funcName := callee.Value
	symbol := sa.findSymbol(funcName)
	
	if symbol == nil {
		sa.addSemanticError(
			fmt.Sprintf("Función '%s' no está declarada", funcName),
			callee.Line,
			callee.Position,
			"UNDEFINED_FUNCTION",
			funcName,
			"FUNCTION",
		)
		return
	}
	
	if symbol.Kind != models.FUNCTION_SYMBOL {
		sa.addSemanticError(
			fmt.Sprintf("'%s' no es una función", funcName),
			callee.Line,
			callee.Position,
			"NOT_A_FUNCTION",
			funcName,
			"IDENTIFIER",
		)
		return
	}
	
	// Marcar función como usada
	symbolKey := sa.getSymbolKeyInScope(funcName, symbol.Scope)
	if sym, exists := sa.symbolTable[symbolKey]; exists {
		sym.Used = true
		sa.symbolTable[symbolKey] = sym
	}
}

// checkAssignment verifica asignaciones
func (sa *SemanticAnalyzer) checkAssignment(node models.SyntaxNode) {
	if len(node.Children) != 2 {
		return
	}
	
	left := node.Children[0]
	right := node.Children[1]
	
	if left.Type != models.IDENTIFIER_EXPRESSION {
		return
	}
	
	varName := left.Value
	symbol := sa.findSymbol(varName)
	
	if symbol == nil {
		sa.addSemanticError(
			fmt.Sprintf("Variable '%s' no está declarada", varName),
			left.Line,
			left.Position,
			"UNDEFINED_VARIABLE",
			varName,
			"IDENTIFIER",
		)
		return
	}
	
	// Verificar si es constante
	if symbol.Attributes["kind"] == "const" {
		sa.addSemanticError(
			fmt.Sprintf("No se puede asignar a la constante '%s'", varName),
			left.Line,
			left.Position,
			"CONST_ASSIGNMENT",
			varName,
			"CONST",
		)
		return
	}
	
	// Verificar compatibilidad de tipos
	leftType := symbol.Type
	rightType := sa.getExpressionType(right)
	
	if !sa.areTypesCompatible(leftType, rightType, "=") {
		sa.addSemanticError(
			fmt.Sprintf("No se puede asignar tipo '%s' a variable de tipo '%s'", 
				rightType, leftType),
			node.Line,
			node.Position,
			"TYPE_MISMATCH",
			varName,
			"ASSIGNMENT",
		)
	}
}

// checkUnusedSymbols verifica símbolos no utilizados
func (sa *SemanticAnalyzer) checkUnusedSymbols() {
	for key, symbol := range sa.symbolTable {
		if !symbol.Used && !sa.usedSymbols[key] {
			// Solo advertir para variables y funciones, no para clases e interfaces
			if symbol.Kind == models.VARIABLE_SYMBOL || symbol.Kind == models.FUNCTION_SYMBOL {
				sa.addSemanticWarning(
					fmt.Sprintf("%s '%s' está declarado pero no se usa", 
						strings.Title(string(symbol.Kind)), symbol.Name),
					symbol.Line,
					symbol.Position,
					"UNUSED_SYMBOL",
					symbol.Name,
					string(symbol.Kind),
				)
			}
		}
	}
}

// Métodos auxiliares

// enterScope entra a un nuevo scope
func (sa *SemanticAnalyzer) enterScope(scopeName string) {
	sa.scopes = append(sa.scopes, scopeName)
	sa.currentScope = scopeName
}

// exitScope sale del scope actual
func (sa *SemanticAnalyzer) exitScope() {
	if len(sa.scopes) > 1 {
		sa.scopes = sa.scopes[:len(sa.scopes)-1]
		sa.currentScope = sa.scopes[len(sa.scopes)-1]
	}
}

// getSymbolKey genera una clave única para el símbolo
func (sa *SemanticAnalyzer) getSymbolKey(name string) string {
	return fmt.Sprintf("%s.%s", sa.currentScope, name)
}

// getSymbolKeyInScope genera una clave para un scope específico
func (sa *SemanticAnalyzer) getSymbolKeyInScope(name, scope string) string {
	return fmt.Sprintf("%s.%s", scope, name)
}

// findSymbol busca un símbolo en los scopes actuales
func (sa *SemanticAnalyzer) findSymbol(name string) *models.Symbol {
	// Buscar en scope actual hacia arriba
	for i := len(sa.scopes) - 1; i >= 0; i-- {
		key := fmt.Sprintf("%s.%s", sa.scopes[i], name)
		if symbol, exists := sa.symbolTable[key]; exists {
			return &symbol
		}
	}
	return nil
}

// inferType infiere el tipo de una expresión
func (sa *SemanticAnalyzer) inferType(node models.SyntaxNode) string {
	switch node.Type {
	case models.LITERAL_EXPRESSION:
		if dataType, exists := node.Attributes["dataType"]; exists {
			return dataType
		}
		return "any"
	case models.IDENTIFIER_EXPRESSION:
		if symbol := sa.findSymbol(node.Value); symbol != nil {
			return symbol.Type
		}
		return "any"
	case models.BINARY_EXPRESSION:
		return sa.getBinaryExpressionType(node)
	default:
		return "any"
	}
}

// getExpressionType obtiene el tipo de una expresión
func (sa *SemanticAnalyzer) getExpressionType(node models.SyntaxNode) string {
	return sa.inferType(node)
}

// getBinaryExpressionType obtiene el tipo de una expresión binaria
func (sa *SemanticAnalyzer) getBinaryExpressionType(node models.SyntaxNode) string {
	if len(node.Children) != 2 {
		return "any"
	}
	
	operator := node.Value
	leftType := sa.getExpressionType(node.Children[0])
	rightType := sa.getExpressionType(node.Children[1])
	
	// Operadores de comparación siempre retornan boolean
	if operator == "==" || operator == "!=" || operator == "<" || 
	   operator == ">" || operator == "<=" || operator == ">=" {
		return "boolean"
	}
	
	// Operadores lógicos retornan boolean
	if operator == "&&" || operator == "||" {
		return "boolean"
	}
	
	// Operadores aritméticos
	if operator == "+" || operator == "-" || operator == "*" || 
	   operator == "/" || operator == "%" {
		// Si alguno es string y el operador es +, es concatenación
		if (leftType == "string" || rightType == "string") && operator == "+" {
			return "string"
		}
		// Si ambos son números, retorna number
		if leftType == "number" && rightType == "number" {
			return "number"
		}
	}
	
	return "any"
}

// areTypesCompatible verifica si dos tipos son compatibles para una operación
func (sa *SemanticAnalyzer) areTypesCompatible(leftType, rightType, operator string) bool {
	// any es compatible con todo
	if leftType == "any" || rightType == "any" {
		return true
	}
	
	// Tipos iguales son compatibles
	if leftType == rightType {
		return true
	}
	
	// Reglas específicas por operador
	switch operator {
	case "+":
		// String concatenation
		return leftType == "string" || rightType == "string"
	case "-", "*", "/", "%":
		// Solo números
		return leftType == "number" && rightType == "number"
	case "==", "!=":
		// Comparación flexible
		return true
	case "<", ">", "<=", ">=":
		// Solo números o strings
		return (leftType == "number" && rightType == "number") ||
			   (leftType == "string" && rightType == "string")
	case "&&", "||":
		// Lógicos aceptan cualquier tipo (se convierten a boolean)
		return true
	case "=":
		// Asignación: tipos compatibles
		return sa.isAssignableType(leftType, rightType)
	}
	
	return false
}

// isAssignableType verifica si rightType se puede asignar a leftType
func (sa *SemanticAnalyzer) isAssignableType(leftType, rightType string) bool {
	if leftType == rightType {
		return true
	}
	
	// any acepta cualquier tipo
	if leftType == "any" {
		return true
	}
	
	// number acepta number
	if leftType == "number" && rightType == "number" {
		return true
	}
	
	// string acepta string
	if leftType == "string" && rightType == "string" {
		return true
	}
	
	// boolean acepta boolean
	if leftType == "boolean" && rightType == "boolean" {
		return true
	}
	
	return false
}

// addSemanticError agrega un error semántico
func (sa *SemanticAnalyzer) addSemanticError(message string, line, position int, errorType, symbol, symbolType string) {
	sa.semanticErrors = append(sa.semanticErrors, models.SemanticError{
		Message:    message,
		Line:       line,
		Position:   position,
		ErrorType:  errorType,
		Symbol:     symbol,
		SymbolType: symbolType,
		Severity:   "ERROR",
	})
}

// addSemanticWarning agrega una advertencia semántica
func (sa *SemanticAnalyzer) addSemanticWarning(message string, line, position int, errorType, symbol, symbolType string) {
	sa.semanticErrors = append(sa.semanticErrors, models.SemanticError{
		Message:    message,
		Line:       line,
		Position:   position,
		ErrorType:  errorType,
		Symbol:     symbol,
		SymbolType: symbolType,
		Severity:   "WARNING",
	})
}