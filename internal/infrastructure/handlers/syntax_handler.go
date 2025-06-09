package handlers

import (
	"encoding/json"
	"fmt"
	"net/http"
	"lexical-analyzer/internal/application/usecases"
	"lexical-analyzer/internal/domain/models"
)

// SyntaxHandler maneja las peticiones HTTP para análisis sintáctico/semántico
type SyntaxHandler struct {
	syntaxUseCase *usecases.SyntaxUseCase
}

// NewSyntaxHandler crea una nueva instancia del handler sintáctico
func NewSyntaxHandler() *SyntaxHandler {
	return &SyntaxHandler{
		syntaxUseCase: usecases.NewSyntaxUseCase(),
	}
}

// AnalyzeSyntax maneja las peticiones de análisis sintáctico y semántico
func (sh *SyntaxHandler) AnalyzeSyntax(w http.ResponseWriter, r *http.Request) {
	// Configurar CORS
	sh.setCORSHeaders(w)
	
	if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
		return
	}
	
	if r.Method != "POST" {
		sh.writeErrorResponse(w, "Método no permitido", http.StatusMethodNotAllowed)
		return
	}
	
	// Decodificar petición
	var request models.SyntaxAnalysisRequest
	if err := json.NewDecoder(r.Body).Decode(&request); err != nil {
		sh.writeErrorResponse(w, "Error al decodificar la petición JSON", http.StatusBadRequest)
		return
	}
	
	// Validar entrada
	if err := sh.validateRequest(request); err != nil {
		sh.writeErrorResponse(w, err.Error(), http.StatusBadRequest)
		return
	}
	
	// Ejecutar análisis
	response := sh.syntaxUseCase.AnalyzeSyntax(request)
	
	// Configurar headers de respuesta
	w.Header().Set("Content-Type", "application/json")
	
	// Codificar y enviar respuesta
	if err := json.NewEncoder(w).Encode(response); err != nil {
		sh.writeErrorResponse(w, "Error al codificar la respuesta", http.StatusInternalServerError)
		return
	}
}

// GetAST obtiene solo el AST del código
func (sh *SyntaxHandler) GetAST(w http.ResponseWriter, r *http.Request) {
	sh.setCORSHeaders(w)
	
	if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
		return
	}
	
	if r.Method != "POST" {
		sh.writeErrorResponse(w, "Método no permitido", http.StatusMethodNotAllowed)
		return
	}
	
	var request models.SyntaxAnalysisRequest
	if err := json.NewDecoder(r.Body).Decode(&request); err != nil {
		sh.writeErrorResponse(w, "Error al decodificar la petición", http.StatusBadRequest)
		return
	}
	
	response := sh.syntaxUseCase.AnalyzeSyntax(request)
	
	// Responder solo con el AST
	astResponse := map[string]interface{}{
		"ast":     response.AST,
		"isValid": response.IsValid,
		"summary": sh.syntaxUseCase.GetSyntaxSummary(response.AST),
	}
	
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(astResponse)
}

// GetSymbolTable obtiene solo la tabla de símbolos
func (sh *SyntaxHandler) GetSymbolTable(w http.ResponseWriter, r *http.Request) {
	sh.setCORSHeaders(w)
	
	if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
		return
	}
	
	if r.Method != "POST" {
		sh.writeErrorResponse(w, "Método no permitido", http.StatusMethodNotAllowed)
		return
	}
	
	var request models.SyntaxAnalysisRequest
	if err := json.NewDecoder(r.Body).Decode(&request); err != nil {
		sh.writeErrorResponse(w, "Error al decodificar la petición", http.StatusBadRequest)
		return
	}
	
	response := sh.syntaxUseCase.AnalyzeSyntax(request)
	
	// Responder solo con la tabla de símbolos
	symbolResponse := map[string]interface{}{
		"symbolTable":    response.SymbolTable,
		"symbolCount":    len(response.SymbolTable),
		"semanticErrors": response.SemanticErrors,
	}
	
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(symbolResponse)
}

// ValidateStructure valida una estructura específica
func (sh *SyntaxHandler) ValidateStructure(w http.ResponseWriter, r *http.Request) {
	sh.setCORSHeaders(w)
	
	if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
		return
	}
	
	if r.Method != "POST" {
		sh.writeErrorResponse(w, "Método no permitido", http.StatusMethodNotAllowed)
		return
	}
	
	// Estructura de petición específica para validación
	var request struct {
		Code          string `json:"code"`
		StructureType string `json:"structureType"` // "for_loop", "function", etc.
	}
	
	if err := json.NewDecoder(r.Body).Decode(&request); err != nil {
		sh.writeErrorResponse(w, "Error al decodificar la petición", http.StatusBadRequest)
		return
	}
	
	// Realizar análisis completo primero
	analysisRequest := models.SyntaxAnalysisRequest{Code: request.Code}
	response := sh.syntaxUseCase.AnalyzeSyntax(analysisRequest)
	
	// Validar estructura específica
	structureErrors := sh.syntaxUseCase.ValidateSpecificStructure(
		response.AST, 
		request.StructureType,
	)
	
	// Respuesta especializada
	validationResponse := map[string]interface{}{
		"structureType":   request.StructureType,
		"isValid":         len(structureErrors) == 0,
		"structureErrors": structureErrors,
		"generalErrors":   response.SyntaxErrors,
		"ast":            response.AST,
	}
	
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(validationResponse)
}

// Health verifica el estado del servicio sintáctico
func (sh *SyntaxHandler) Health(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	
	healthResponse := map[string]interface{}{
		"status":   "ok",
		"service":  "syntax-analyzer",
		"features": []string{
			"lexical-analysis",
			"syntax-analysis", 
			"semantic-analysis",
			"symbol-table",
			"ast-generation",
		},
	}
	
	json.NewEncoder(w).Encode(healthResponse)
}

// Métodos auxiliares

// setCORSHeaders configura los headers CORS
func (sh *SyntaxHandler) setCORSHeaders(w http.ResponseWriter) {
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")
	w.Header().Set("Access-Control-Max-Age", "3600")
}

// validateRequest valida la petición de entrada
func (sh *SyntaxHandler) validateRequest(request models.SyntaxAnalysisRequest) error {
	if request.Code == "" {
		return fmt.Errorf("el campo 'code' es requerido y no puede estar vacío")
	}
	
	// Validar longitud máxima del código
	maxCodeLength := 100000 // 100KB
	if len(request.Code) > maxCodeLength {
		return fmt.Errorf("el código excede la longitud máxima permitida (%d caracteres)", maxCodeLength)
	}
	
	return nil
}

// writeErrorResponse escribe una respuesta de error
func (sh *SyntaxHandler) writeErrorResponse(w http.ResponseWriter, message string, statusCode int) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(statusCode)
	
	errorResponse := map[string]interface{}{
		"error":   true,
		"message": message,
		"status":  statusCode,
	}
	
	json.NewEncoder(w).Encode(errorResponse)
}

// writeSuccessResponse escribe una respuesta exitosa
func (sh *SyntaxHandler) writeSuccessResponse(w http.ResponseWriter, data interface{}) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	
	response := map[string]interface{}{
		"success": true,
		"data":    data,
	}
	
	json.NewEncoder(w).Encode(response)
}