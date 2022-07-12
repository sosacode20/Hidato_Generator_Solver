# Hidato

Un hidato es un tablero que puede tener distintas formas y que se identifica por poseer un menor y un mayor elemento marcados en el tablero, donde el objetivo es llegar desde el menor al mayor y para eso también se tienen otros números extras en el mismo con el fin de ayudar al jugador a conseguir la meta y a la vez con el fin de que el juego tenga solución única

## Estructura del proyecto

El proyecto de Haskell esta creado con el gestor de paquetes llamado *Cabal* y contiene 2 ejecutables... Uno para generar los tableros (*MainGenerator*) y otro para solucionarlos (*MainSolver*)

### Componentes básicas comunes a los 2 ejecutables

#### Representación del tablero

El tablero del Hidato se representa con el *Record* **HidatoBoard** que tiene 2 constructores:

- Board
- Empty

Se ve de la siguiente forma:

```haskell
data HidatoBoard = Board 
    { board :: [[Int]]
    , minVal :: Int
    , maxVal :: Int
    } | Empty deriving (Show, Eq)
```

Es decir, un tablero de Hidato lo identificamos en el código como una matriz de enteros donde el -1 son las posiciones bloqueadas y los ceros son las posiciones a rellenar y los números positivos son los que están colocados como ayuda para solucionar el Hidato mas fácil
También esta presente en este *Record* la información del menor y el mayor valor

## Solución del Hidato

Para la solución del Hidato usamos un backtracking que inicia en la posición del menor elemento del hidato y hace lo siguiente:

- Comprobamos primeramente que sea valido el tablero:
  - Si el tablero no es valido
- Luego comprobamos si el tablero actual ya esta solucionado, en cuyo caso se devuelve el mismo.
- Si no esta solucionado y es valido entonces se llama recursivo a otro método con las direcciones siguientes validas a explorar y con el numero a colocar:
  - En este método se hace una poda que comprueba si el numero siguiente a colocar esta en el tablero y no es adyacente, en cuyo caso se sale del backtrack como que por esa via no hay solución y si es adyacente solo se explora por esa posicion
- De la forma en que esta construido este metodo siempre se va a llegar a uno de los casos bases dados (o se invalida el tablero o se encontro una solucion) y la primera solucion encontrada es la que se devuelve

## Generador de tableros

Para el generador usamos la siguiente idea:
Creamos un tablero vacio con las dimensiones dadas por el usuario y las casillas invalidas.

Ponemos el minimo y el maximo a partir de aqui se crea un Hidato.
Teniendo el Hidato Board se hace lo siguiente:
- Tenemos en el archivo Hidato Generator un predicado semejante al Solver que soluciona el hidato. Este devuelve si hay solucion y si hay mas de una, ademas de una lista de celdas que hay que colocar en el tablero inicial para llegar a la solucion escogida.
- Tenemos un predicado para generar un hidato unico que depende del solver descrito anteriormente:
  - A partir del board en el estado inicial llama al solver modificado y si el solver no tiene solucion se devuelve NOTHING
  - Si tiene solucion entonces se comprueba si hay una o mas de una
  - Si solo hay una solucion se devuelve el tablero a partir de donde se encotro la solucion
  - Si hay mas de una solucion se toma de la lista de celdas a colocar un numero random de ellas y se colocan como fijos en el tablero y se vuelve a llamar recursivo al Generate Hidato con este nuevo tablero.
  - Al finalizar se tuvo que haber alcanzado un Hidato con solucion unica o que no es valido.
