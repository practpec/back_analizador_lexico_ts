package main

import (
	"fmt"
	"log"
	"net/http"
	"lexical-analyzer/internal/infrastructure/handlers"
)

// main inicializa y ejecuta el servidor HTTP
func main() {
	// Crear instancia del handler
	analyzerHandler := handlers.NewAnalyzerHandler()
	
	// Configurar rutas
	http.HandleFunc("/api/analyze", analyzerHandler.AnalyzeCode)
	http.HandleFunc("/api/health", analyzerHandler.Health)
	
	// Configurar servidor
	port := ":8080"
	
	fmt.Printf("🚀 Servidor del analizador léxico iniciado en puerto %s\n", port)
	fmt.Println("📍 Endpoints disponibles:")
	fmt.Println("   POST /api/analyze - Analizar código")
	fmt.Println("   GET  /api/health  - Verificar estado")
	
	// Iniciar servidor
	if err := http.ListenAndServe(port, nil); err != nil {
		log.Fatal("Error al iniciar el servidor:", err)
	}
}