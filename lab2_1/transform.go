package main

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"os"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Println("usage: transform <file.go>")
		return
	}
	filename := os.Args[1]

	// Создаём FileSet для учёта позиций в исходном коде
	fset := token.NewFileSet()

	// Парсим исходный файл
	file, err := parser.ParseFile(fset, filename, nil, parser.ParseComments)
	if err != nil {
		fmt.Println("Error parsing file:", err)
		return
	}

	// Обходим все функции в файле и модифицируем их тела
	for _, decl := range file.Decls {
		if funcDecl, ok := decl.(*ast.FuncDecl); ok && funcDecl.Body != nil {
			transformBody(funcDecl.Body)
		}
	}

	// Выводим преобразованный код
	conf := printer.Config{Mode: printer.TabIndent | printer.UseSpaces, Tabwidth: 4}
	err = conf.Fprint(os.Stdout, fset, file)
	if err != nil {
		fmt.Println("Error printing transformed code:", err)
	}
}

// transformBody рекурсивно обходит операторы внутри block.List и
// заменяет все for-циклы на блоки вида:
//
//	{
//	    var __labCounter int
//	    for ... {
//	        __labCounter++
//	        ...
//	    }
//	    fmt.Println("Iterations:", __labCounter)
//	}
//
// Плюс, если внутри есть другие блоки (if { ... }, switch { case ... }, и т.д.),
// мы также рекурсивно заходим в них, чтобы не пропустить вложенные циклы.
func transformBody(block *ast.BlockStmt) {
	for i := 0; i < len(block.List); i++ {
		stmt := block.List[i]

		switch s := stmt.(type) {
		// Если это for-цикл, обрабатываем
		case *ast.ForStmt:
			// Сначала рекуривно преобразуем тело самого цикла,
			// чтобы там тоже найти и изменить вложенные for.
			transformBody(s.Body)

			// Создаём оператор: var __labCounter int
			declStmt := &ast.DeclStmt{
				Decl: &ast.GenDecl{
					Tok: token.VAR,
					Specs: []ast.Spec{
						&ast.ValueSpec{
							Names:  []*ast.Ident{ast.NewIdent("__labCounter")},
							Type:   ast.NewIdent("int"),
							Values: nil,
						},
					},
				},
			}

			// Создаём оператор __labCounter++
			incStmt := &ast.IncDecStmt{
				X:   ast.NewIdent("__labCounter"),
				Tok: token.INC, // ++
			}

			// Вставляем __labCounter++ в начало тела цикла
			newBody := make([]ast.Stmt, 0, len(s.Body.List)+1)
			newBody = append(newBody, incStmt)
			newBody = append(newBody, s.Body.List...)
			s.Body.List = newBody

			// Создаём оператор вывода счётчика
			printStmt := &ast.ExprStmt{
				X: &ast.CallExpr{
					Fun: &ast.SelectorExpr{
						X:   ast.NewIdent("fmt"),
						Sel: ast.NewIdent("Println"),
					},
					Args: []ast.Expr{
						&ast.BasicLit{
							Kind:  token.STRING,
							Value: "\"Iterations:\"",
						},
						ast.NewIdent("__labCounter"),
					},
				},
			}

			// Оборачиваем всё в новый блок
			newBlock := &ast.BlockStmt{
				List: []ast.Stmt{
					declStmt,
					s,         // сам цикл
					printStmt, // вывод счётчика
				},
			}

			// Заменяем исходный оператор for на newBlock
			block.List[i] = newBlock

		// Если встретили вложенный блок (например, внутри if { ... }), тоже спускаемся вглубь.
		case *ast.BlockStmt:
			transformBody(s)

		case *ast.IfStmt:
			if s.Body != nil {
				transformBody(s.Body)
			}
			if s.Else != nil {
				if elseBlock, ok := s.Else.(*ast.BlockStmt); ok {
					transformBody(elseBlock)
				} else if elseIf, ok := s.Else.(*ast.IfStmt); ok {
					// Рекурсивно зовём transformBody у elseIf.Body и т.д.
					if elseIf.Body != nil {
						transformBody(elseIf.Body)
					}
				}
			}

		case *ast.SwitchStmt:
			transformBody(s.Body)
		}
	}
}
