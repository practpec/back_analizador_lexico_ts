package usecases

import (
	"fmt"
	"lexical-analyzer/internal/domain/models"
	"lexical-analyzer/internal/domain/services"
)

// SyntaxUseCase maneja los casos de uso del analizador sintáctico/semántico
type SyntaxUseCase struct {
	syntaxService *services.SyntaxService
}

// NewSyntaxUseCase crea una nueva instancia del caso de uso sintáctico
func NewSyntaxUseCase() *SyntaxUseCase {
	return &SyntaxUseCase{
		syntaxService: services.NewSyntaxService(),
	}
}

// AnalyzeSyntax ejecuta el análisis sintáctico y semántico completo
func (suc *SyntaxUseCase) AnalyzeSyntax(request models.SyntaxAnalysisRequest) models.SyntaxAnalysisResponse {
	if request.Code == "" {
		return models.SyntaxAnalysisResponse{
			AST: models.SyntaxNode{
				Type:     models.SYNTAX_ERROR_NODE,
				Value:    "Código vacío",
				Line:     1,
				Position: 1,
			},
			SyntaxErrors: []models.SyntaxError{
				{
					Message:   "No se proporcionó código para analizar",
					Line:      1,
					Position:  1,
					ErrorType: "EMPTY_CODE",
					Severity:  "ERROR",
				},
			},
			SemanticErrors: []models.SemanticError{},
			SymbolTable:    []models.Symbol{},
			IsValid:        false,
		}
	}
	
	// Realizar análisis completo
	result := suc.syntaxService.AnalyzeComplete(request.Code)
	
	// Validar estructura adicional si no hay errores críticos
	if result.IsValid || len(result.SyntaxErrors) == 0 {
		structureErrors := suc.syntaxService.ValidateStructure(result.AST)
		if len(structureErrors) > 0 {
			result.SyntaxErrors = append(result.SyntaxErrors, structureErrors...)
			result.IsValid = false
		}
	}
	
	// Agregar metadatos del análisis
	result = suc.enrichResponse(result)
	
	return result
}

// enrichResponse enriquece la respuesta con información adicional
func (suc *SyntaxUseCase) enrichResponse(response models.SyntaxAnalysisResponse) models.SyntaxAnalysisResponse {
	// Agregar información estadística
	summary := suc.syntaxService.GetASTSummary(response.AST)
	
	// Agregar atributos al nodo raíz del AST
	if response.AST.Attributes == nil {
		response.AST.Attributes = make(map[string]string)
	}
	
	response.AST.Attributes["analysis_complete"] = "true"
	response.AST.Attributes["syntax_errors_count"] = fmt.Sprintf("%d", len(response.SyntaxErrors))
	response.AST.Attributes["semantic_errors_count"] = fmt.Sprintf("%d", len(response.SemanticErrors))
	response.AST.Attributes["symbols_count"] = fmt.Sprintf("%d", len(response.SymbolTable))
	
	if depth, ok := summary["depth"].(int); ok {
		response.AST.Attributes["ast_depth"] = fmt.Sprintf("%d", depth)
	}
	
	// Clasificar errores por severidad
	response = suc.classifyErrors(response)
	
	return response
}

// classifyErrors clasifica los errores por severidad y tipo
func (suc *SyntaxUseCase) classifyErrors(response models.SyntaxAnalysisResponse) models.SyntaxAnalysisResponse {
	// Contar errores por severidad
	syntaxErrorCount := 0
	syntaxWarningCount := 0
	
	for _, err := range response.SyntaxErrors {
		if err.Severity == "ERROR" {
			syntaxErrorCount++
		} else if err.Severity == "WARNING" {
			syntaxWarningCount++
		}
	}
	
	semanticErrorCount := 0
	semanticWarningCount := 0
	
	for _, err := range response.SemanticErrors {
		if err.Severity == "ERROR" {
			semanticErrorCount++
		} else if err.Severity == "WARNING" {
			semanticWarningCount++
		}
	}
	
	// Agregar información de clasificación al AST
	if response.AST.Attributes == nil {
		response.AST.Attributes = make(map[string]string)
	}
	
	response.AST.Attributes["syntax_errors"] = fmt.Sprintf("%d", syntaxErrorCount)
	response.AST.Attributes["syntax_warnings"] = fmt.Sprintf("%d", syntaxWarningCount)
	response.AST.Attributes["semantic_errors"] = fmt.Sprintf("%d", semanticErrorCount)
	response.AST.Attributes["semantic_warnings"] = fmt.Sprintf("%d", semanticWarningCount)
	
	// Determinar estado general
	if syntaxErrorCount > 0 || semanticErrorCount > 0 {
		response.AST.Attributes["status"] = "error"
	} else if syntaxWarningCount > 0 || semanticWarningCount > 0 {
		response.AST.Attributes["status"] = "warning"
	} else {
		response.AST.Attributes["status"] = "success"
	}
	
	return response
}

// GetSyntaxSummary obtiene un resumen del análisis sintáctico
func (suc *SyntaxUseCase) GetSyntaxSummary(ast models.SyntaxNode) map[string]interface{} {
	return suc.syntaxService.GetASTSummary(ast)
}

// ValidateSpecificStructure valida estructuras específicas como loops
func (suc *SyntaxUseCase) ValidateSpecificStructure(ast models.SyntaxNode, structureType string) []models.SyntaxError {
	var errors []models.SyntaxError
	
	switch structureType {
	case "for_loop":
		suc.validateForLoopStructure(ast, &errors)
	case "function":
		suc.validateFunctionStructure(ast, &errors)
	case "variable_declaration":
		suc.validateVariableStructure(ast, &errors)
	}
	
	return errors
}

// validateForLoopStructure valida específicamente la estructura de loops for
func (suc *SyntaxUseCase) validateForLoopStructure(node models.SyntaxNode, errors *[]models.SyntaxError) {
	if node.Type == models.FOR_STATEMENT {
		// Verificar que tenga la estructura correcta para el ejemplo del usuario
		// for (int=1; i<=10; i++)
		
		if len(node.Children) < 4 {
			*errors = append(*errors, models.SyntaxError{
				Message:   "Loop 'for' debe tener inicialización, condición, incremento y cuerpo",
				Line:      node.Line,
				Position:  node.Position,
				ErrorType: "INCOMPLETE_FOR_LOOP",
				Expected:  "for(init; condition; increment) { body }",
				Found:     "estructura incompleta",
				Severity:  "ERROR",
			})
		}
		
		// Validar inicialización (primer hijo)
		if len(node.Children) > 0 {
			init := node.Children[0]
			if init.Type != models.VARIABLE_DECLARATION && init.Type != models.EXPRESSION_STATEMENT {
				*errors = append(*errors, models.SyntaxError{
					Message:   "Inicialización del loop 'for' debe ser una declaración o expresión",
					Line:      init.Line,
					Position:  init.Position,
					ErrorType: "INVALID_FOR_INIT",
					Severity:  "ERROR",
				})
			}
		}
		
		// Validar condición (segundo hijo)
		if len(node.Children) > 1 {
			condition := node.Children[1]
			if condition.Type != models.BINARY_EXPRESSION && condition.Type != models.IDENTIFIER_EXPRESSION {
				*errors = append(*errors, models.SyntaxError{
					Message:   "Condición del loop 'for' debe ser una expresión booleana",
					Line:      condition.Line,
					Position:  condition.Position,
					ErrorType: "INVALID_FOR_CONDITION",
					Severity:  "ERROR",
				})
			}
		}
		
		// Validar incremento (tercer hijo)
		if len(node.Children) > 2 {
			increment := node.Children[2]
			if increment.Type != models.EXPRESSION_STATEMENT && increment.Type != models.UNARY_EXPRESSION {
				*errors = append(*errors, models.SyntaxError{
					Message:   "Incremento del loop 'for' debe ser una expresión",
					Line:      increment.Line,
					Position:  increment.Position,
					ErrorType: "INVALID_FOR_INCREMENT",
					Severity:  "ERROR",
				})
			}
		}
		
		// Validar cuerpo (cuarto hijo)
		if len(node.Children) > 3 {
			body := node.Children[3]
			if body.Type != models.BLOCK_STATEMENT && body.Type != models.EXPRESSION_STATEMENT {
				*errors = append(*errors, models.SyntaxError{
					Message:   "Cuerpo del loop 'for' debe ser un bloque o expresión",
					Line:      body.Line,
					Position:  body.Position,
					ErrorType: "INVALID_FOR_BODY",
					Severity:  "ERROR",
				})
			}
		}
	}
	
	// Recursivamente validar hijos
	for _, child := range node.Children {
		suc.validateForLoopStructure(child, errors)
	}
}

// validateFunctionStructure valida la estructura de funciones
func (suc *SyntaxUseCase) validateFunctionStructure(node models.SyntaxNode, errors *[]models.SyntaxError) {
	if node.Type == models.FUNCTION_DECLARATION {
		// Verificar componentes básicos de la función
		hasName := false
		hasBody := false
		
		for _, child := range node.Children {
			if child.Type == models.IDENTIFIER_EXPRESSION {
				hasName = true
			}
			if child.Type == models.BLOCK_STATEMENT {
				hasBody = true
			}
		}
		
		if !hasName {
			*errors = append(*errors, models.SyntaxError{
				Message:   "Función debe tener un nombre",
				Line:      node.Line,
				Position:  node.Position,
				ErrorType: "MISSING_FUNCTION_NAME",
				Severity:  "ERROR",
			})
		}
		
		if !hasBody {
			*errors = append(*errors, models.SyntaxError{
				Message:   "Función debe tener un cuerpo",
				Line:      node.Line,
				Position:  node.Position,
				ErrorType: "MISSING_FUNCTION_BODY",
				Severity:  "ERROR",
			})
		}
	}
	
	// Recursivamente validar hijos
	for _, child := range node.Children {
		suc.validateFunctionStructure(child, errors)
	}
}

// validateVariableStructure valida la estructura de declaraciones de variables
func (suc *SyntaxUseCase) validateVariableStructure(node models.SyntaxNode, errors *[]models.SyntaxError) {
	if node.Type == models.VARIABLE_DECLARATION {
		// Verificar que tenga al menos un identificador
		if len(node.Children) == 0 {
			*errors = append(*errors, models.SyntaxError{
				Message:   "Declaración de variable debe tener un identificador",
				Line:      node.Line,
				Position:  node.Position,
				ErrorType: "MISSING_VARIABLE_NAME",
				Severity:  "ERROR",
			})
		} else if node.Children[0].Type != models.IDENTIFIER_EXPRESSION {
			*errors = append(*errors, models.SyntaxError{
				Message:   "Primer elemento de declaración de variable debe ser un identificador",
				Line:      node.Children[0].Line,
				Position:  node.Children[0].Position,
				ErrorType: "INVALID_VARIABLE_NAME",
				Severity:  "ERROR",
			})
		}
		
		// Verificar inicialización para constantes
		if node.Attributes != nil && node.Attributes["kind"] == "const" {
			hasInitializer := false
			for _, child := range node.Children {
				if child.Type == models.LITERAL_EXPRESSION || 
				   child.Type == models.BINARY_EXPRESSION ||
				   child.Type == models.CALL_EXPRESSION {
					hasInitializer = true
					break
				}
			}
			
			if !hasInitializer {
				*errors = append(*errors, models.SyntaxError{
					Message:   "Constante debe ser inicializada",
					Line:      node.Line,
					Position:  node.Position,
					ErrorType: "CONST_WITHOUT_INITIALIZER",
					Severity:  "ERROR",
				})
			}
		}
	}
	
	// Recursivamente validar hijos
	for _, child := range node.Children {
		suc.validateVariableStructure(child, errors)
	}
}