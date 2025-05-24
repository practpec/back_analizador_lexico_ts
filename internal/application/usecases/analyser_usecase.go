package usecases

import (
	"lexical-analyzer/internal/domain/models"
	"lexical-analyzer/internal/domain/services"
)

// AnalyzerUseCase maneja los casos de uso del analizador léxico
type AnalyzerUseCase struct {
	lexicalService *services.LexicalService
}

// NewAnalyzerUseCase crea una nueva instancia del caso de uso
func NewAnalyzerUseCase() *AnalyzerUseCase {
	return &AnalyzerUseCase{
		lexicalService: services.NewLexicalService(),
	}
}

// AnalyzeCode ejecuta el análisis léxico del código proporcionado
func (auc *AnalyzerUseCase) AnalyzeCode(request models.AnalysisRequest) models.AnalysisResponse {
	if request.Code == "" {
		return models.AnalysisResponse{
			Tokens: []models.Token{},
			Errors: []models.Error{
				{
					Message:  "No se proporcionó código para analizar",
					Line:     1,
					Position: 1,
				},
			},
		}
	}
	
	return auc.lexicalService.Analyze(request.Code)
}