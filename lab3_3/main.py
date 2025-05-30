import sys
import os
from parser import parse_spec
from semantic_analyzer import analyze, SemanticError

def main():
    if len(sys.argv) < 2:
        print("Использование: python main.py <путь_к_файлу_спецификации>")
        print("Для запуска тестов используйте: python test_semantic_analyzer.py")
        return 1
    
    filename = sys.argv[1]
    if not os.path.isfile(filename):
        print(f"Ошибка: файл '{filename}' не существует")
        return 1
    
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            spec_text = f.read()
    except Exception as e:
        print(f"Ошибка при чтении файла: {e}")
        return 1
    
    # Синтаксический анализ
    spec = parse_spec(spec_text)
    if not spec:
        # Ошибка парсинга, сообщение уже выведено в parse_spec
        return 1
    
    # Семантический анализ
    try:
        analyze(spec)
        print("Программа корректна")
        return 0
    except SemanticError as e:
        print(f"Семантическая ошибка ({e.pos}): {e.message}")
        return 1

if __name__ == "__main__":
    sys.exit(main()) 