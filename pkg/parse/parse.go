package parse

import (
	"context"
	"errors"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"sort"
	"strings"

	sitter "github.com/smacker/go-tree-sitter"
	"github.com/smacker/go-tree-sitter/golang"
)

const C = "\"C\""

type GciImports struct {
	// original index of import group, include doc, name, path and comment
	Start, End int
	Name, Path string
}
type ImportList []*GciImports

func (l ImportList) Len() int {
	return len(l)
}

func (l ImportList) Less(i, j int) bool {
	if strings.Compare(l[i].Path, l[j].Path) == 0 {
		return strings.Compare(l[i].Name, l[j].Name) < 0
	}

	return strings.Compare(l[i].Path, l[j].Path) < 0
}

func (l ImportList) Swap(i, j int) { l[i], l[j] = l[j], l[i] }

/*
 * AST considers a import block as below:
 * ```
 * Doc
 * Name Path Comment
 * ```
 * An example is like below:
 * ```
 * // test
 * test "fmt" // test
 * ```
 * getImports return a import block with name, start and end index
 */
func getImports(imp *ast.ImportSpec) (start, end int, name string) {
	if imp.Doc != nil {
		// doc poc need minus one to get the first index of comment
		start = int(imp.Doc.Pos()) - 1
	} else {
		if imp.Name != nil {
			// name pos need minus one too
			start = int(imp.Name.Pos()) - 1
		} else {
			// path pos start without quote, need minus one for it
			start = int(imp.Path.Pos()) - 1
		}
	}

	if imp.Name != nil {
		name = imp.Name.Name
	}

	if imp.Comment != nil {
		end = int(imp.Comment.End())
	} else {
		end = int(imp.Path.End())
	}
	return
}

func ParseFile(src []byte, filename string) (ImportList, int, int, int, int, error) {
	parser := sitter.NewParser()
	parser.SetLanguage(golang.GetLanguage())
	tree, err := parser.ParseCtx(context.Background(), nil, src)
	if err != nil {
		return nil, 0, 0, 0, 0, fmt.Errorf("ParseCtx: %w", err)
	}

	query, err := sitter.NewQuery([]byte("(import_declaration) @imports"), golang.GetLanguage())
	if err != nil {
		return nil, 0, 0, 0, 0, err
	}
	declarationQueryCursor := sitter.NewQueryCursor()
	declarationQueryCursor.Exec(query, tree.RootNode())

	var (
		// headEnd means the start of import block
		headEnd int
		// tailStart means the end + 1 of import block
		tailStart int
		// cStart means the start of C import block
		cStart int
		// cEnd means the end of C import block
		cEnd int
		data ImportList
	)

	query, err = sitter.NewQuery([]byte("(import_spec) @import"), golang.GetLanguage())
	if err != nil {
		return nil, 0, 0, 0, 0, fmt.Errorf("building query to get 'import_spec's in 'import_declaration's: %w", err)
	}

	var declarationIndex int
	for declarationMatch, ok := declarationQueryCursor.NextMatch(); ok; declarationMatch, ok = declarationQueryCursor.NextMatch() {
		importDeclaration := declarationMatch.Captures[0].Node
		if importDeclaration.HasError() {
			return nil, 0, 0, 0, 0, errors.New("import_declaration contains errors")
		}

		if headEnd == 0 {
			headEnd = int(importDeclaration.StartByte())
		}
		tailStart = int(importDeclaration.EndByte() + 1)
		if tailStart > len(src) {
			tailStart = len(src)
		}


		specQueryCursor := sitter.NewQueryCursor()
		specQueryCursor.Exec(query, importDeclaration)

		var prevSpec *sitter.Node

		for specMatch, ok := specQueryCursor.NextMatch(); ok; specMatch, ok = specQueryCursor.NextMatch() {
			importSpec := specMatch.Captures[0].Node

			start := importSpec.StartByte()
			commentSiblingNode := importSpec
			if parent := importSpec.Parent(); parent.Type() == "import_declaration" {
				commentSiblingNode = parent
			}
			for prevSibling := commentSiblingNode.PrevSibling(); prevSibling != nil && prevSibling.Type() == "comment" && (prevSpec == nil || prevSpec.StartPoint().Row != prevSibling.StartPoint().Row); prevSibling = prevSibling.PrevSibling() {
				start = prevSibling.StartByte()
			}

			end := importSpec.EndByte() + 1
			if nextSibling := importSpec.NextSibling(); nextSibling != nil && nextSibling.Type() == "comment" && (nextSibling.StartPoint().Row == importSpec.StartPoint().Row) {
				end = nextSibling.EndByte() + 1
			}

			var name string
			if nameNode := importSpec.ChildByFieldName("name"); nameNode != nil {
				name = nameNode.Content(src)
			}

			path := strings.Trim(importSpec.ChildByFieldName("path").Content(src), `"`)

			if path == "C" {
				cStart = int(start)
				cEnd = int(end)

				// lines like 'import "C"' with no doc comment
				if commentSiblingNode.Type() == "import_declaration" && start == importSpec.StartByte() {
					cStart = int(commentSiblingNode.StartByte())
				}

				if declarationIndex == 0 {
					headEnd = cStart
				}
			} else {
				data = append(
					data,
					&GciImports{
						Start: int(start),
						End:   int(end),
						Name:  strings.TrimSpace(name),
						Path:  path,
					},
				)
			}

			prevSpec = importSpec
			declarationIndex++
		}
	}

	sort.Sort(data)
	return data, headEnd, tailStart, cStart, cEnd, nil
}

func printData(data ImportList) {
	fmt.Printf("printing %d records\n", len(data))
	for _, d := range data {
		fmt.Printf("Start: %d End: %d Name: %s Path: %s\n", d.Start, d.End, d.Name, d.Path)
	}
}

func ParseFiles(src []byte, filename string) (ImportList, int, int, int, int, error) {
	fileSet := token.NewFileSet()
	f, err := parser.ParseFile(fileSet, filename, src, parser.ParseComments)
	if err != nil {
		return nil, 0, 0, 0, 0, err
	}

	if len(f.Imports) == 0 {
		return nil, 0, 0, 0, 0, NoImportError{}
	}

	var (
		// headEnd means the start of import block
		headEnd int
		// tailStart means the end + 1 of import block
		tailStart int
		// cStart means the start of C import block
		cStart int
		// cEnd means the end of C import block
		cEnd int
		data ImportList
	)

	for index, decl := range f.Decls {
		switch genDecl := decl.(type) {
		// skip BadDecl and FuncDecl
		case *ast.GenDecl:
			if genDecl.Tok == token.IMPORT {
				// there are two cases, both end with linebreak:
				// 1.
				// import (
				//	 "xxxx"
				// )
				// 2.
				// import "xxx"
				if headEnd == 0 {
					headEnd = int(decl.Pos()) - 1
				}
				tailStart = int(decl.End())
				if tailStart > len(src) {
					tailStart = len(src)
				}

				for _, spec := range genDecl.Specs {
					imp := spec.(*ast.ImportSpec)
					// there are only one C import block
					// ensure C import block is the first import block
					if imp.Path.Value == C {
						/*
							common case:

							// #include <png.h>
							import "C"

							notice that decl.Pos() == genDecl.Pos() > genDecl.Doc.Pos()
						*/
						if genDecl.Doc != nil {
							cStart = int(genDecl.Doc.Pos()) - 1
							// if C import block is the first, update headEnd
							if index == 0 {
								headEnd = cStart
							}
						} else {
							/*
								special case:

								import "C"
							*/
							cStart = int(decl.Pos()) - 1
						}

						cEnd = int(decl.End())

						continue
					}

					start, end, name := getImports(imp)

					data = append(data, &GciImports{
						Start: start,
						End:   end,
						Name:  name,
						Path:  strings.Trim(imp.Path.Value, `"`),
					})
				}
			}
		}
	}

	sort.Sort(data)
	return data, headEnd, tailStart, cStart, cEnd, nil
}

// IsGeneratedFileByComment reports whether the source file is generated code.
// Using a bit laxer rules than https://golang.org/s/generatedcode to
// match more generated code.
// Taken from https://github.com/golangci/golangci-lint.
func IsGeneratedFileByComment(in string) bool {
	const (
		genCodeGenerated = "code generated"
		genDoNotEdit     = "do not edit"
		genAutoFile      = "autogenerated file"      // easyjson
		genAutoGenerated = "automatically generated" // genny
	)

	markers := []string{genCodeGenerated, genDoNotEdit, genAutoFile, genAutoGenerated}
	in = strings.ToLower(in)
	for _, marker := range markers {
		if strings.Contains(in, marker) {
			return true
		}
	}

	return false
}

type NoImportError struct{}

func (n NoImportError) Error() string {
	return "No imports"
}

func (i NoImportError) Is(err error) bool {
	_, ok := err.(NoImportError)
	return ok
}
