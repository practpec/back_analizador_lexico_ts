package main

import (
	"fmt"
	"log"
	"net/http"
	"lexical-analyzer/internal/infrastructure/handlers"
)

// main inicializa y ejecuta el servidor HTTP con análisis léxico, sintáctico y semántico
func main() {
	// Crear instancias de los handlers
	analyzerHandler := handlers.NewAnalyzerHandler()
	syntaxHandler := handlers.NewSyntaxHandler()
	
	// Configurar rutas del analizador léxico
	http.HandleFunc("/api/analyze", analyzerHandler.AnalyzeCode)
	http.HandleFunc("/api/health", analyzerHandler.Health)
	
	// Configurar rutas del analizador sintáctico/semántico
	http.HandleFunc("/api/syntax/analyze", syntaxHandler.AnalyzeSyntax)
	http.HandleFunc("/api/syntax/ast", syntaxHandler.GetAST)
	http.HandleFunc("/api/syntax/symbols", syntaxHandler.GetSymbolTable)
	http.HandleFunc("/api/syntax/validate", syntaxHandler.ValidateStructure)
	http.HandleFunc("/api/syntax/health", syntaxHandler.Health)
	
	// Configurar servidor
	port := ":8080"
	
	fmt.Printf("🚀 Servidor del analizador léxico/sintáctico/semántico iniciado en puerto %s\n", port)
	fmt.Println("📍 Endpoints disponibles:")
	fmt.Println("   === ANÁLISIS LÉXICO ===")
	fmt.Println("   POST /api/analyze - Análizar tokens (léxico)")
	fmt.Println("   GET  /api/health  - Estado del servicio léxico")
	fmt.Println("")
	fmt.Println("   === ANÁLISIS SINTÁCTICO/SEMÁNTICO ===")
	fmt.Println("   POST /api/syntax/analyze  - Análisis completo (sintáctico + semántico)")
	fmt.Println("   POST /api/syntax/ast      - Obtener solo el AST")
	fmt.Println("   POST /api/syntax/symbols  - Obtener tabla de símbolos")
	fmt.Println("   POST /api/syntax/validate - Validar estructura específica")
	fmt.Println("   GET  /api/syntax/health   - Estado del servicio sintáctico")
	fmt.Println("")
	fmt.Println("📋 Ejemplos de uso:")
	fmt.Println("   curl -X POST http://localhost:8080/api/syntax/analyze \\")
	fmt.Println("     -H \"Content-Type: application/json\" \\")
	fmt.Println("     -d '{\"code\":\"for(let i=1; i<=10; i++) { console.log(i); }\"}'")
	
	// Iniciar servidor
	if err := http.ListenAndServe(port, nil); err != nil {
		log.Fatal("Error al iniciar el servidor:", err)
	}
}