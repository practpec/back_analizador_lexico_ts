package handlers

import (
	"encoding/json"
	"net/http"
	"lexical-analyzer/internal/application/usecases"
	"lexical-analyzer/internal/domain/models"
)

type AnalyzerHandler struct {
	analyzerUseCase *usecases.AnalyzerUseCase
}

func NewAnalyzerHandler() *AnalyzerHandler {
	return &AnalyzerHandler{
		analyzerUseCase: usecases.NewAnalyzerUseCase(),
	}
}

func (ah *AnalyzerHandler) AnalyzeCode(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Content-Type", "application/json")
	
	if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
		return
	}
	
	if r.Method != "POST" {
		http.Error(w, "Método no permitido", http.StatusMethodNotAllowed)
		return
	}
	
	var request models.AnalysisRequest
	if err := json.NewDecoder(r.Body).Decode(&request); err != nil {
		http.Error(w, "Error al decodificar la petición", http.StatusBadRequest)
		return
	}
	
	response := ah.analyzerUseCase.AnalyzeCode(request)
	
	if err := json.NewEncoder(w).Encode(response); err != nil {
		http.Error(w, "Error al codificar la respuesta", http.StatusInternalServerError)
		return
	}
}

func (ah *AnalyzerHandler) Health(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(http.StatusOK)
	json.NewEncoder(w).Encode(map[string]string{"status": "ok"})
}