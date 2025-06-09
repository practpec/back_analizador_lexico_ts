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

// isValidIdentifierChar verifica si un carácter es válido para identificadores (incluyendo Unicode)
func (ls *LexicalService) isValidIdentifierChar(char rune) bool {
	// Permitir letras Unicode, números y guión bajo
	return unicode.IsLetter(char) || unicode.IsDigit(char) || char == '_'
}

// isValidIdentifierStart verifica si un carácter puede iniciar un identificador
func (ls *LexicalService) isValidIdentifierStart(char rune) bool {
	// Permitir letras Unicode y guión bajo para iniciar
	return unicode.IsLetter(char) || char == '_'
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
		
		// Verificar comentarios de línea (//)
		if i < len(line)-1 && line[i] == '/' && line[i+1] == '/' {
			comment, newPos := ls.extractComment(line, i, lineNum)
			tokens = append(tokens, comment)
			i = newPos
			continue
		}
		
		// Verificar comentarios de bloque (/* */)
		if i < len(line)-1 && line[i] == '/' && line[i+1] == '*' {
			comment, newPos := ls.extractBlockComment(line, i, lineNum)
			tokens = append(tokens, comment)
			i = newPos
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
		
		// Verificar identificadores y palabras reservadas (ahora con soporte Unicode)
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
		
		// Verificar delimitadores y operadores (excepto : que puede ser parte de tipos)
		if ls.isDelimiterOrOperator(rune(line[i])) {
			// Manejo especial para operadores compuestos
			if i < len(line)-1 {
				twoChar := line[i:i+2]
				if ls.isTwoCharOperator(twoChar) {
					token := models.Token{
						Type:     models.OPERATOR,
						Value:    twoChar,
						Line:     lineNum,
						Position: i + 1,
					}
					tokens = append(tokens, token)
					i += 2
					continue
				}
			}
			
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

// extractComment extrae un comentario de línea completo
func (ls *LexicalService) extractComment(line string, start int, lineNum int) (models.Token, int) {
	// Un comentario de línea va hasta el final de la línea
	comment := line[start:]
	
	return models.Token{
		Type:     models.COMMENT,
		Value:    comment,
		Line:     lineNum,
		Position: start + 1,
	}, len(line)
}

// extractBlockComment extrae un comentario de bloque
func (ls *LexicalService) extractBlockComment(line string, start int, lineNum int) (models.Token, int) {
	end := start + 2 // Empezar después de /*
	
	// Buscar */
	for end < len(line)-1 {
		if line[end] == '*' && line[end+1] == '/' {
			end += 2 // Incluir */
			break
		}
		end++
	}
	
	// Si no encontramos el cierre en esta línea, tomar hasta el final
	if end >= len(line)-1 && !(end >= 2 && line[end-2:end] == "*/") {
		end = len(line)
	}
	
	return models.Token{
		Type:     models.COMMENT,
		Value:    line[start:end],
		Line:     lineNum,
		Position: start + 1,
	}, end
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

// extractIdentifier extrae un identificador o palabra reservada válida (con soporte Unicode)
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
	delimiters := "(){}[];,."
	operators := "+-*/=<>!&|^%~:"
	
	return strings.ContainsRune(delimiters+operators, char)
}

// isTwoCharOperator verifica si es un operador de dos caracteres
func (ls *LexicalService) isTwoCharOperator(op string) bool {
	twoCharOps := []string{"==", "!=", "<=", ">=", "&&", "||", "++", "--", "+=", "-=", "*=", "/="}
	for _, twoChar := range twoCharOps {
		if op == twoChar {
			return true
		}
	}
	return false
}

func (ls *LexicalService) getDelimiterOperatorType(char rune) models.TokenType {
	delimiters := "(){}[];,."
	if strings.ContainsRune(delimiters, char) {
		return models.DELIMITER
	}
	return models.OPERATOR
}

func initializeReservedWords() map[string]bool {
	words := []string{
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