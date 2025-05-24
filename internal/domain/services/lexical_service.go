package services

import (
	"strings"
	"unicode"
	"lexical-analyzer/internal/domain/models"
)

// LexicalService implementa la lógica de análisis léxico
type LexicalService struct {
	reservedWords map[string]bool
}

// NewLexicalService crea una nueva instancia del servicio léxico
func NewLexicalService() *LexicalService {
	return &LexicalService{
		reservedWords: initializeReservedWords(),
	}
}

// Analyze realiza el análisis léxico del código proporcionado
func (ls *LexicalService) Analyze(code string) models.AnalysisResponse {
	tokens := []models.Token{}
	errors := []models.Error{}
	
	lines := strings.Split(code, "\n")
	
	for lineNum, line := range lines {
		lineTokens, lineErrors := ls.analyzeLine(line, lineNum+1)
		tokens = append(tokens, lineTokens...)
		errors = append(errors, lineErrors...)
	}
	
	return models.AnalysisResponse{
		Tokens: tokens,
		Errors: errors,
	}
}

// isValidIdentifierChar verifica si un carácter es válido para identificadores
func (ls *LexicalService) isValidIdentifierChar(char rune) bool {
	// Solo permitir letras ASCII, números y guión bajo
	return (char >= 'a' && char <= 'z') ||
		   (char >= 'A' && char <= 'Z') ||
		   (char >= '0' && char <= '9') ||
		   char == '_'
}

// isValidIdentifierStart verifica si un carácter puede iniciar un identificador
func (ls *LexicalService) isValidIdentifierStart(char rune) bool {
	// Solo permitir letras ASCII y guión bajo para iniciar
	return (char >= 'a' && char <= 'z') ||
		   (char >= 'A' && char <= 'Z') ||
		   char == '_'
}

// analyzeLine analiza una línea individual de código
func (ls *LexicalService) analyzeLine(line string, lineNum int) ([]models.Token, []models.Error) {
	tokens := []models.Token{}
	errors := []models.Error{}
	
	i := 0
	for i < len(line) {
		// Saltar espacios en blanco
		if unicode.IsSpace(rune(line[i])) {
			i++
			continue
		}
		
		// Verificar números
		if unicode.IsDigit(rune(line[i])) {
			token, newPos := ls.extractNumber(line, i, lineNum)
			tokens = append(tokens, token)
			i = newPos
			continue
		}
		
		// Verificar strings (comillas dobles y simples)
		if line[i] == '"' || line[i] == '\'' {
			token, newPos, err := ls.extractString(line, i, lineNum)
			if err != nil {
				errors = append(errors, *err)
			} else {
				tokens = append(tokens, token)
			}
			i = newPos
			continue
		}
		
		// Verificar identificadores y palabras reservadas
		if ls.isValidIdentifierStart(rune(line[i])) {
			token, newPos, err := ls.extractIdentifier(line, i, lineNum)
			if err != nil {
				errors = append(errors, *err)
			} else {
				tokens = append(tokens, token)
			}
			i = newPos
			continue
		}
		
		// Verificar delimitadores y operadores (sin comillas, ya procesadas arriba)
		if ls.isDelimiterOrOperator(rune(line[i])) {
			token := models.Token{
				Type:     ls.getDelimiterOperatorType(rune(line[i])),
				Value:    string(line[i]),
				Line:     lineNum,
				Position: i + 1,
			}
			tokens = append(tokens, token)
			i++
			continue
		}
		
		// Manejar identificadores inválidos que contienen caracteres especiales
		if unicode.IsLetter(rune(line[i])) || line[i] == '_' {
			invalidToken, newPos := ls.extractInvalidIdentifier(line, i, lineNum)
			errors = append(errors, models.Error{
				Message:  "Identificador inválido: '" + invalidToken + "' contiene caracteres no permitidos",
				Line:     lineNum,
				Position: i + 1,
			})
			i = newPos
			continue
		}
		
		// Carácter no reconocido - error de sintaxis
		errors = append(errors, models.Error{
			Message:  "Carácter no válido: '" + string(line[i]) + "'",
			Line:     lineNum,
			Position: i + 1,
		})
		i++
	}
	
	return tokens, errors
}

// extractString extrae un string completo (entre comillas) y detecta strings sin cerrar
func (ls *LexicalService) extractString(line string, start int, lineNum int) (models.Token, int, *models.Error) {
	quote := line[start] // Puede ser " o '
	end := start + 1
	
	// Buscar la comilla de cierre
	for end < len(line) && line[end] != quote {
		// Manejar caracteres escapados
		if line[end] == '\\' && end+1 < len(line) {
			end += 2 // Saltar el carácter escapado
		} else {
			end++
		}
	}
	
	// Verificar si encontramos la comilla de cierre
	if end >= len(line) {
		// String sin cerrar - esto es un error
		return models.Token{}, end, &models.Error{
			Message:  "String sin cerrar: falta comilla de cierre '" + string(quote) + "'",
			Line:     lineNum,
			Position: start + 1,
		}
	}
	
	// Incluir la comilla de cierre
	end++
	
	return models.Token{
		Type:     models.STRING,
		Value:    line[start:end],
		Line:     lineNum,
		Position: start + 1,
	}, end, nil
}
func (ls *LexicalService) extractInvalidIdentifier(line string, start int, lineNum int) (string, int) {
	end := start
	
	// Continuar mientras sea letra, número, guión bajo O caracteres especiales
	for end < len(line) {
		char := rune(line[end])
		if unicode.IsSpace(char) || ls.isDelimiterOrOperator(char) {
			break
		}
		end++
	}
	
	return line[start:end], end
}

// extractNumber extrae un número completo de la línea
func (ls *LexicalService) extractNumber(line string, start int, lineNum int) (models.Token, int) {
	end := start
	hasDecimal := false
	
	for end < len(line) && (unicode.IsDigit(rune(line[end])) || line[end] == '.') {
		if line[end] == '.' {
			if hasDecimal {
				break // Segundo punto decimal, terminar
			}
			hasDecimal = true
		}
		end++
	}
	
	return models.Token{
		Type:     models.NUMBER,
		Value:    line[start:end],
		Line:     lineNum,
		Position: start + 1,
	}, end
}

// extractIdentifier extrae un identificador o palabra reservada válida
func (ls *LexicalService) extractIdentifier(line string, start int, lineNum int) (models.Token, int, *models.Error) {
	end := start
	
	// Verificar primer carácter
	if !ls.isValidIdentifierStart(rune(line[start])) {
		return models.Token{}, start + 1, &models.Error{
			Message:  "Identificador no puede comenzar con: '" + string(line[start]) + "'",
			Line:     lineNum,
			Position: start + 1,
		}
	}
	
	// Extraer el resto del identificador
	for end < len(line) && ls.isValidIdentifierChar(rune(line[end])) {
		end++
	}
	
	// Verificar si hay caracteres inválidos después
	if end < len(line) && !unicode.IsSpace(rune(line[end])) && !ls.isDelimiterOrOperator(rune(line[end])) {
		// Encontrar el final del token inválido
		invalidEnd := end
		for invalidEnd < len(line) {
			char := rune(line[invalidEnd])
			if unicode.IsSpace(char) || ls.isDelimiterOrOperator(char) {
				break
			}
			invalidEnd++
		}
		
		return models.Token{}, invalidEnd, &models.Error{
			Message:  "Identificador inválido: '" + line[start:invalidEnd] + "' contiene caracteres no permitidos",
			Line:     lineNum,
			Position: start + 1,
		}
	}
	
	value := line[start:end]
	tokenType := models.IDENTIFIER
	
	if ls.reservedWords[value] {
		tokenType = models.RESERVED_WORD
	} else {
		// Verificar si es una palabra reservada mal escrita
		if ls.isMisspelledReservedWord(value) {
			return models.Token{}, end, &models.Error{
				Message:  "Palabra reservada mal escrita: '" + value + "' (¿quisiste decir '" + ls.suggestCorrection(value) + "'?)",
				Line:     lineNum,
				Position: start + 1,
			}
		}
	}
	
	return models.Token{
		Type:     tokenType,
		Value:    value,
		Line:     lineNum,
		Position: start + 1,
	}, end, nil
}

// isMisspelledReservedWord verifica si una palabra es una palabra reservada mal escrita
func (ls *LexicalService) isMisspelledReservedWord(word string) bool {
	commonMisspellings := map[string]string{
		"fora":     "for",
		"iff":      "if",
		"elsee":    "else",
		"whilee":   "while",
		"truee":    "true",
		"falsee":   "false",
		"returnn":  "return",
		"functionn": "function",
		"classs":   "class",
		"constt":   "const",
		"lett":     "let",
		"varr":     "var",
	}
	
	_, exists := commonMisspellings[word]
	return exists
}

// suggestCorrection sugiere la corrección para una palabra mal escrita
func (ls *LexicalService) suggestCorrection(word string) string {
	corrections := map[string]string{
		"fora":     "for",
		"iff":      "if", 
		"elsee":    "else",
		"whilee":   "while",
		"truee":    "true",
		"falsee":   "false",
		"returnn":  "return",
		"functionn": "function",
		"classs":   "class",
		"constt":   "const",
		"lett":     "let",
		"varr":     "var",
	}
	
	if correction, exists := corrections[word]; exists {
		return correction
	}
	return word
}

// isDelimiterOrOperator verifica si un carácter es delimitador u operador
func (ls *LexicalService) isDelimiterOrOperator(char rune) bool {
	delimiters := "(){}[];,.\"'"  // Agregamos comillas dobles y simples
	operators := "+-*/=<>!&|^%~"
	
	return strings.ContainsRune(delimiters+operators, char)
}

// getDelimiterOperatorType determina el tipo de token para delimitadores y operadores
func (ls *LexicalService) getDelimiterOperatorType(char rune) models.TokenType {
	delimiters := "(){}[];,.\"'"  // Agregamos comillas dobles y simples
	
	if strings.ContainsRune(delimiters, char) {
		return models.DELIMITER
	}
	return models.OPERATOR
}

// initializeReservedWords inicializa el mapa de palabras reservadas de TypeScript
func initializeReservedWords() map[string]bool {
	words := []string{
		// Palabras clave básicas
		"abstract", "any", "as", "async", "await", "boolean", "break", "case", "catch",
		"class", "const", "constructor", "continue", "debugger", "declare", "default",
		"delete", "do", "else", "enum", "export", "extends", "false", "finally",
		"for", "from", "function", "get", "if", "implements", "import", "in",
		"instanceof", "interface", "is", "keyof", "let", "module", "namespace",
		"never", "new", "null", "number", "object", "of", "package", "private",
		"protected", "public", "readonly", "return", "set", "static", "string",
		"super", "switch", "symbol", "this", "throw", "true", "try", "type",
		"typeof", "undefined", "union", "var", "void", "while", "with", "yield",
	}
	
	wordMap := make(map[string]bool)
	for _, word := range words {
		wordMap[word] = true
	}
	
	return wordMap
}