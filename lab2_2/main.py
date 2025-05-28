import sys
from parser import parse_spec

def main():
    if len(sys.argv) > 1:
        filename = sys.argv[1]
        with open(filename, 'r') as f:
            spec_text = f.read()
            spec = parse_spec(spec_text)
            if spec:
                print("\nРезультаты разбора спецификации:")
                print("===============================")
                print(f"Имя класса: {spec.class_name}")
                print("\nТокены:")
                for token in spec.tokens:
                    print(f"  {token}")
                
                print("\nТипы:")
                for type_mapping in spec.types:
                    names = ', '.join(type_mapping.names)
                    print(f"  {names}: {type_mapping.type}")
                
                print("\nМетоды:")
                for method in spec.methods:
                    params = []
                    for param in method.parameters:
                        params.append(str(param.type_name))
                    param_str = ', '.join(params)
                    print(f"  {method.return_type} {method.name}({param_str})")
                
                print("\nПравила грамматики:")
                for rule in spec.grammar:
                    print(f"  {rule.name} =")
                    for i, alt in enumerate(rule.alternatives):
                        if i > 0:
                            print("    |", end=" ")
                        else:
                            print("      ", end="")
                        
                        for elem in alt.elements:
                            if isinstance(elem, IdRuleElement):
                                print(elem.name, end=" ")
                            elif isinstance(elem, RepRuleElement):
                                print("%rep", end=" ")
                                if isinstance(elem.element, IdRuleElement):
                                    print(elem.element.name, end=" ")
                                elif isinstance(elem.element, ParenRuleElement):
                                    print("(...)", end=" ")
                            elif isinstance(elem, ParenRuleElement):
                                print("(...)", end=" ")
                        
                        if alt.action:
                            print(f"/ {alt.action}", end="")
                        print()
                    print()
                
                print(f"Аксиома: {spec.axiom}")
    else:
        print("Использование: python main.py <файл_спецификации>")

if __name__ == "__main__":
    main() 