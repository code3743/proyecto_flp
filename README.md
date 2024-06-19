## Proyecto final - Fundamentos de Lenguajes de Programación (FLP) - 2024-1

### Integrantes:
- Bernal, Pedro - 2259548-3743
- López, Jota - 2259394-3743
- Rivas, Esmeralda - 2259580-3743

### Descripción:
El objetivo de este proyecto es desarrollar un intérprete para un lenguaje de programación [específico](grammar.rkt), aprovechando las capacidades de Racket y la librería SLLGEN. Racket, junto con SLLGEN, facilita la generación de analizadores léxicos y sintácticos, proporcionando un entorno ideal para este desarrollo. El lenguaje de programación implementado incluye un subconjunto de características, tales como:
- Tipos de datos: Números (decimales, enteros, binarios, octales y hexadecimales),booleanos, cadenas, arreglos y listas.
- Estructuras de control: if, else, while, for, switch y match.
- Funciones: definición y llamado de funciones.
- Estructuras de datos: structs
- Operadores: aritméticos, lógicos, relación y de asignación.

### Ejecución de pruebas

Para ejecutar todas las pruebas:

```bash
sh test.sh all
```

Para ejecutar pruebas de forma unitaria:

```bash
sh test.sh number
sh test.sh arrays
sh test.sh cadena
sh test.sh ligatures
sh test.sh control_structures
sh test.sh functions
sh test.sh data_structures
sh test.sh match
```
