# Cuatro en raya
Práctica para la asignatura LP de la FIB que sustituye el examen parcial de Haskell en el Q2 2019-2020. [Enunciat de la pràctica. ](https://github.com/jordi-petit/lp-quatre-en-ratlla)

La practica consiste en implementar el juego de mesa Cuatro en raya junto con 3 bots para el juego. 
* El primero hace movimientos aleatorios.
* El segundo hace movimientos greedy evitando que el usuario gane si uno de sus posibles movimientos greedy evita una raya de 4 del usuario.
* El tercero hace movimientos inteligentes.

## Compilación y ejecución
Para compilar el juego es necesario utilizar el siguiente comando:
```
ghc Practica.hs
```

Y una vez tenemos el ejecutable, para iniciar el juego ejecutamos: 

```
./Practica
```

## Opciones iniciales
Al iniciar la partida se pediran una serie de parametros. 
* Primero las dimensiones,los valores han de ser positivos y al menos el valor de las filas o el de las columnas ha de ser mayor que 4. Justo despues se mostrará el tablero. Si alguna de las condiciones no se cumple, se mostrará un mensaje de error y volveran a pedirse las dimensiones.

    ```
    > Elige el número de filas del tablero
    6
    > Elige el número de columnas del tablero
    7

    |_|_|_|_|_|_|_|
    |_|_|_|_|_|_|_|
    |_|_|_|_|_|_|_|
    |_|_|_|_|_|_|_|
    |_|_|_|_|_|_|_|
    |_|_|_|_|_|_|_|
    ```

* A continuacion se pedira la estrategia del bot con la que jugar. Asegurate de poner uno de los valores mostrados o se mostrará un mensaje de error. Y se volverá a pedir la estrategia a utilizar.
    ```
    > Que estrategia quieres que el bot tenga?: 1.Random 2.Greedy 3.Smart
    1
    ```
* Y finalmente se pedira quien comienza la partida, el bot escogido o el usuario. Si se introduce un número diferente a los permitidos, se volverá a pedir quien debe empezar la partida.

    ```
    > Quieres tener el primer movimiento (escribe 1) o el segundo (escribe 2)?
    > Recuerda que el bot siempre sera las X y tu los O?
    1
    ```

## Como jugar
En este juego se escoge la columna en la que se quiere poner una ficha, en cuanto se haya escogido, se colocará una ficha en la parte vacía mas inferior de la columna.
Las columnas estan numeradas desde el 1 hasta `n` siendo `n` el número de columnas de la tabla. Estas serán las posiciones en las que colocar  una ficha.<br><br>
La partida acabará en cuanto alguno de los 2 jugadores haya formado una linea de longitud 4 o mayor en el tablero. O si el tablero se acaba llenando y ninguno ha hecho un 4 en raya, es decir, hay un empate. <br><br>
Cada vez que sea el turno del usuario para poner ficha, se mostrará el mensaje:

```
>Elige en que columna quieres poner la ficha entre la 1 y la `n`
1
```
En este momento el usuario ha de poner un número válido de columna donde colocar la ficha, en el caso de que no sea válido, se mostrará el mensaje
```
>La posicion x es inválida
```
Donde `x` será el número introducido por el usuario.
Y se volverá a pedir que se introduzca un número de columna válido.<br>
Despues de que el usuario haya colocado una ficha se mostrará el nuevo estado del tablero junto con una flecha indicando a que tablero corresponde el movimiento para evitar confusiones.

```
>Elige en que columna quieres poner la ficha entre la 1 y la 7

4
>
|_|_|_|_|_|_|_|
|_|_|_|_|_|_|_|
|_|_|_|_|_|_|_|
|_|_|_|_|_|_|_| 
|_|_|_|_|_|_|_|
|_|_|_|O|_|_|_|

>Has colocado ficha en la columna 4 ↖
```
Y si el bot ha hecho un movimiento:
```
>
|_|_|_|_|_|_|_|
|_|_|_|_|_|_|_|
|_|_|_|_|_|_|_|
|_|_|_|_|_|_|_|
|_|_|_|_|_|_|_|
|_|X|_|O|_|_|_|

>El bot ha colocado ficha en la columna 2 ↖
```

## Los bots
### Random
El primer bot es bastante sencillo de explicar, coloca fichas aleatoriamente sobre posiciones validas del tablero.
### Greedy
El bot tirará fichas en aquellas posiciones que le permita hacer la raya mas grande posible, en caso de empate, tirará de forma aleatoria sobre una de las columnas.   Si ademas se da el caso de que una de estas posiciones puede evitar que el usuario haga un 4 en raya, entonces el bot tirará alli. El bot no intentará bloquear un 4 en raya del usuario si eso no le permite hacer la linea mas grande posible. (es como comprendí el enunciado)
### Smart
El bot smart tiene escondida una estrategia greedy mejorada respecto a la del bot anterior. Sus prioridades son:
* Si el bot puede ganar con algún movimiento válido haciendo un 4 en raya, hará ese movimiento.
* Si el usuario puede ganar con algun movimiento válido, el bot intentará bloquear ese movimiento.

* Si se da la situación en la que hay una columna en la que si el usuario pone una ficha, y si seguidamente el bot pusiera una ficha sobre esa misma columna pudiese darle la victoria, el bot evitará poner una ficha en la columna. El bot hará lo mismo si se da el caso de que el usuario puede ganar poniendo una ficha en una columna si el bot pone antes coloca una ficha en esa columna. <br><br>
   A continuación se muestran los ejemplos mencionados, en ambos casos, el bot evitará poner una ficha en la columna 4 durante el resto de la partida a no ser que el usuario ponga una ficha alli antes.
  ```
    |_|_|_|_|_|_|_|     |_|_|_|_|_|_|_|
    |_|_|_|_|_|_|_|     |_|_|_|_|_|_|_|
    |_|_|_|_|_|_|_|     |_|_|_|_|_|_|_|
    |_|X|_|_|_|_|_|     |_|O|_|_|_|_|_|
    |O|O|O|_|_|_|_|     |X|X|X|_|_|_|_|
    |X|O|X|_|_|_|_|     |O|X|O|_|_|_|_|
  ```
  Las siguientes condiciones tienen en cuenta esta ultima restricción.<br>
* Si existe la posibilidad de que el usuario pueda conseguir un 4 en raya haciendo dos movimientos, el bot hará aquel movimiento que le permita al usuario empezar el 4 en raya.
* En el caso de que esa posibilidad no exista, llegamos al caso predeterminado en el que el bot hará un movimiento greedy, pero será más inteligente de como lo hacia el bot anterior. El bot ahora tendrá en cuenta que exista la posibilidad de hacer 4 en raya en alguna situación futura del tablero, y le da un valor a ese movimiento correspondiente a la linea mas grande que se pueda hacer. Si no existe la posibilidad de formar una linea de longitud 4 en el futuro, el valor del movimiento greedy, dando igual la longitud de la raya que se pueda hacer, será 0.
* Por lo general, en todas las condiciones anteriores, el bot intentará siempre hacer el movimiento que sea más centrico en el tablero si se da el caso de que hay varias opciones.

## Autor
* Rodrigo Arian Huapaya Sierra