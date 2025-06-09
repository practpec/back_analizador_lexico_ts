package services

import (
	"lexical-analyzer/internal/domain/models"
)

// SyntaxService coordina el análisis sintáctico y semántico
type SyntaxService struct {
	lexicalService *LexicalService
}

// NewSyntaxService crea una nueva instancia del servicio de sintaxis
func NewSyntaxService() *SyntaxService {
	return &SyntaxService{
		lexicalService: NewLexicalService(),
	}
}

// AnalyzeComplete realiza análisis léxico, sintáctico y semántico completo
func (ss *SyntaxService) AnalyzeComplete(code string) models.SyntaxAnalysisResponse {
	// 1. Análisis léxico
	lexicalResult := ss.lexicalService.Analyze(code)
	
	// Si hay errores léxicos críticos, no continuar
	if len(lexicalResult.Errors) > 0 {
		// Verificar si hay errores que impiden el análisis sintáctico
		hasCriticalErrors := false
		for _, err := range lexicalResult.Errors {
			// Errores críticos que impiden continuar
			if err.Message == "String sin cerrar" || 
			   err.Message == "Carácter no válido" {
				hasCriticalErrors = true
				break
			}
		}
		
		if hasCriticalErrors {
			return models.SyntaxAnalysisResponse{
				AST: models.SyntaxNode{
					Type:  models.SYNTAX_ERROR_NODE,
					Value: "Análisis sintáctico cancelado debido a errores léxicos críticos",
				},
				SyntaxErrors: []models.SyntaxError{
					{
						Message:   "No se puede continuar con el análisis sintáctico debido a errores léxicos",
						Line:      1,
						Position:  1,
						ErrorType: "LEXICAL_ERROR",
						Severity:  "ERROR",
					},
				},
				SemanticErrors: []models.SemanticError{},
				SymbolTable:    []models.Symbol{},
				IsValid:        false,
			}
		}
	}
	
	// 2. Análisis sintáctico
	parser := NewSyntaxParser(lexicalResult.Tokens)
	ast := parser.Parse()
	syntaxErrors := parser.GetSyntaxErrors()
	
	// 3. Análisis semántico (solo si no hay errores sintácticos críticos)
	var semanticErrors []models.SemanticError
	var symbolTable []models.Symbol
	
	if len(syntaxErrors) == 0 || !ss.hasCriticalSyntaxErrors(syntaxErrors) {
		semanticAnalyzer := NewSemanticAnalyzer(ast)
		symbolTable, semanticErrors = semanticAnalyzer.Analyze()
	}
	
	// Determinar si el código es válido
	isValid := len(syntaxErrors) == 0 && 
			   len(semanticErrors) == 0 && 
			   len(lexicalResult.Errors) == 0
	
	return models.SyntaxAnalysisResponse{
		AST:            ast,
		SyntaxErrors:   syntaxErrors,
		SemanticErrors: semanticErrors,
		SymbolTable:    symbolTable,
		IsValid:        isValid,
	}
}

// hasCriticalSyntaxErrors verifica si hay errores sintácticos críticos
func (ss *SyntaxService) hasCriticalSyntaxErrors(errors []models.SyntaxError) bool {
	criticalErrorTypes := map[string]bool{
		"MISSING_BRACE":      true,
		"MISSING_SEMICOLON":  false, // No crítico en TypeScript
		"UNEXPECTED_TOKEN":   true,
		"INVALID_SYNTAX":     true,
	}
	
	for _, err := range errors {
		if critical, exists := criticalErrorTypes[err.ErrorType]; exists && critical {
			return true
		}
	}
	
	return false
}

// ValidateStructure valida la estructura básica del código
func (ss *SyntaxService) ValidateStructure(ast models.SyntaxNode) []models.SyntaxError {
	var structureErrors []models.SyntaxError
	
	// Verificar estructura básica del programa
	ss.validateNode(ast, &structureErrors)
	
	return structureErrors
}

// validateNode valida un nodo del AST recursivamente
func (ss *SyntaxService) validateNode(node models.SyntaxNode, errors *[]models.SyntaxError) {
	switch node.Type {
	case models.FUNCTION_DECLARATION:
		ss.validateFunctionDeclaration(node, errors)
	case models.FOR_STATEMENT:
		ss.validateForStatement(node, errors)
	case models.VARIABLE_DECLARATION:
		ss.validateVariableDeclaration(node, errors)
	case models.BLOCK_STATEMENT:
		ss.validateBlockStatement(node, errors)
	}
	
	// Validar hijos recursivamente
	for _, child := range node.Children {
		ss.validateNode(child, errors)
	}
}

// validateFunctionDeclaration valida declaraciones de funciones
func (ss *SyntaxService) validateFunctionDeclaration(node models.SyntaxNode, errors *[]models.SyntaxError) {
	// Verificar que tenga al menos nombre y cuerpo
	if len(node.Children) < 2 {
		*errors = append(*errors, models.SyntaxError{
			Message:   "Declaración de función incompleta",
			Line:      node.Line,
			Position:  node.Position,
			ErrorType: "INCOMPLETE_FUNCTION",
			Severity:  "ERROR",
		})
		return
	}
	
	// Verificar que el primer hijo sea un identificador
	if node.Children[0].Type != models.IDENTIFIER_EXPRESSION {
		*errors = append(*errors, models.SyntaxError{
			Message:   "Se esperaba el nombre de la función",
			Line:      node.Line,
			Position:  node.Position,
			ErrorType: "MISSING_FUNCTION_NAME",
			Severity:  "ERROR",
		})
	}
	
	// Verificar que tenga cuerpo (último hijo debe ser BLOCK_STATEMENT)
	lastChild := node.Children[len(node.Children)-1]
	if lastChild.Type != models.BLOCK_STATEMENT {
		*errors = append(*errors, models.SyntaxError{
			Message:   "Función debe tener un cuerpo",
			Line:      node.Line,
			Position:  node.Position,
			ErrorType: "MISSING_FUNCTION_BODY",
			Severity:  "ERROR",
		})
	}
}

// validateForStatement valida sentencias for
func (ss *SyntaxService) validateForStatement(node models.SyntaxNode, errors *[]models.SyntaxError) {
	// Un for debe tener al menos la condición y el cuerpo
	if len(node.Children) < 2 {
		*errors = append(*errors, models.SyntaxError{
			Message:   "Sentencia for incompleta",
			Line:      node.Line,
			Position:  node.Position,
			ErrorType: "INCOMPLETE_FOR",
			Severity:  "ERROR",
		})
		return
	}
	

	
	if len(node.Children) != 4 {
		*errors = append(*errors, models.SyntaxError{
			Message:   "Sentencia for debe tener inicialización, condición, incremento y cuerpo",
			Line:      node.Line,
			Position:  node.Position,
			ErrorType: "INVALID_FOR_STRUCTURE",
			Expected:  "for(init; condition; increment) body",
			Found:     "estructura incompleta",
			Severity:  "ERROR",
		})
	}
}

// validateVariableDeclaration valida declaraciones de variables
func (ss *SyntaxService) validateVariableDeclaration(node models.SyntaxNode, errors *[]models.SyntaxError) {
	// Verificar que tenga al menos un identificador
	if len(node.Children) == 0 {
		*errors = append(*errors, models.SyntaxError{
			Message:   "Declaración de variable vacía",
			Line:      node.Line,
			Position:  node.Position,
			ErrorType: "EMPTY_DECLARATION",
			Severity:  "ERROR",
		})
		return
	}
	
	// Verificar que el primer hijo sea un identificador
	if node.Children[0].Type != models.IDENTIFIER_EXPRESSION {
		*errors = append(*errors, models.SyntaxError{
			Message:   "Se esperaba un identificador en la declaración de variable",
			Line:      node.Line,
			Position:  node.Position,
			ErrorType: "INVALID_VARIABLE_NAME",
			Severity:  "ERROR",
		})
	}
}

// validateBlockStatement valida bloques de código
func (ss *SyntaxService) validateBlockStatement(node models.SyntaxNode, errors *[]models.SyntaxError) {
	// Los bloques pueden estar vacíos, pero verificar que las llaves estén balanceadas
	// Esto ya debería estar verificado en el parser, pero agregamos validación adicional
	
	// Verificar que no haya statements inválidos dentro del bloque
	for _, child := range node.Children {
		if child.Type == models.SYNTAX_ERROR_NODE {
			*errors = append(*errors, models.SyntaxError{
				Message:   "Error de sintaxis dentro del bloque",
				Line:      child.Line,
				Position:  child.Position,
				ErrorType: "BLOCK_SYNTAX_ERROR",
				Severity:  "ERROR",
			})
		}
	}
}

// GetASTSummary genera un resumen del AST para debugging
func (ss *SyntaxService) GetASTSummary(ast models.SyntaxNode) map[string]interface{} {
	summary := map[string]interface{}{
		"nodeType":    ast.Type,
		"nodeValue":   ast.Value,
		"childCount":  len(ast.Children),
		"hasErrors":   ast.Type == models.SYNTAX_ERROR_NODE,
	}
	
	// Contar tipos de nodos
	nodeTypeCounts := make(map[models.NodeType]int)
	ss.countNodeTypes(ast, nodeTypeCounts)
	
	summary["nodeTypeCounts"] = nodeTypeCounts
	summary["depth"] = ss.calculateASTDepth(ast)
	
	return summary
}

// countNodeTypes cuenta la cantidad de cada tipo de nodo
func (ss *SyntaxService) countNodeTypes(node models.SyntaxNode, counts map[models.NodeType]int) {
	counts[node.Type]++
	for _, child := range node.Children {
		ss.countNodeTypes(child, counts)
	}
}

// calculateASTDepth calcula la profundidad máxima del AST
func (ss *SyntaxService) calculateASTDepth(node models.SyntaxNode) int {
	if len(node.Children) == 0 {
		return 1
	}
	
	maxChildDepth := 0
	for _, child := range node.Children {
		childDepth := ss.calculateASTDepth(child)
		if childDepth > maxChildDepth {
			maxChildDepth = childDepth
		}
	}
	
	return maxChildDepth + 1
}