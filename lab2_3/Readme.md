Запустите парсер из командной строки:

```bash
# Сгенерировать DOT и вывести в консоль
python main.py input_grammar.txt

# Сгенерировать DOT и сохранить в файл
python main.py input_grammar.txt -o output_tree.dot

# (Опционально, если установлен Graphviz) Преобразовать .dot файл в изображение (например, PNG)
dot -Tpng output_tree.dot -o output_tree.png
```

### Пример входного файла (`input_grammar.txt`)

```
% аксиома
[axiom [E]]

% правила грамматики
[E    [T E']]
[E'   [+ T E'] []]
[T    [F T']]
[T'   [* F T'] []]
[F    [n] [( E )]]
```
