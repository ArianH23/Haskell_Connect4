# Cuatro en raya
Práctica para la asignatura LP de la FIB que sustituye el examen parcial de Haskell en el Q2 2019-2020. [Enunciat de la pràctica](https://github.com/jordi-petit/lp-quatre-en-ratlla)

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
Al iniciar la partida se pediran una serie de valores. 
* Primero las dimensiones asegurate de no poner valores negativos o que al menos el valor de filas o de columnas sea mayor que 4.Justo despues se mostrar el tablero. Si alguna de las condiciones no se cumple, se mostrará un mensaje de error y volveran a pedirse las dimensiones

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
    Si por algun motivo los dos números que han sido introducidos son menores que 4, se volverá a pedir que se introduzcan
    el número de filas y columnas, ya que sino el juego no tendría sentido.
* A continuacion se pedira la estrategia. Asegurate de poner uno de los valores mostrados o se mostrr un mensaje de error.
    ```
    > Que estrategia quieres que el bot tenga?: 1.Random 2.Greedy 3.Smart
    1
    ```
    Si se introduce un número diferente, se volverá a pedir que estrategia debe usar el bot.
* Y finalmente se pedira quien comienza la partida, el bot escogido o el usuario.
    ```
    > Quieres tener el primer movimiento (escribe 1) o el segundo (escribe 2)?
    > Recuerda que el bot siempre sera las X y tu los O?
    1
    ```
    Si se introduce un número diferente, se volverá a pedir quien debe empezar la partida.

## Como jugar
En este juego se escoge la columna en la que se quiere poner una ficha, y en cuanto se haya escogido esta se pondra en el fondo de la columna.
Las columnas estan numeradas desde el 1 hasta `n` siendo n el número de columnas de la tabla.<br>
Cada vez que sea el turno del usuario de poner una ficha, se mostrará el mensaje:

```
>Elige en que columna quieres poner la ficha entre la 1 y la `n`
1
```
En este momento el usuario ha de poner un número válido de columna donde colocar la ficha, en el caso de que no sea válido, se mostrará el mensaje
```
>Posición inválida
```
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

>Has puesto ficha en la posición 4 ↖
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

>El bot ha puesto ficha en la posición 2 ↖
```
La partida acabará en cuanto uno de los dos jugadores haya hecho 4 en raya, o bien cuando el tablero este completamente lleno y la partida haya acabado
en empate. En cualquiera de estos casos se mostrará un mensaje diciendo quien ha ganado o si ha habido un empate.

## Los bots
### Random
El primer bot es bastante sencillo de explicar, coloca fichas aleatoriamente sobre posiciones validas del tablero.
### Greedy
Aqui tuve de dudas sobre como interpretar el enunciado. Lo que yo comprendí es que el bot tirará fichas en aquellas posiciones que le permita hacer la raya mas grande posible, si ademas se da el caso de que una de estas posiciones puede evitar que el usuario haga un 4 en raya, entonces el bot tirará alli. El bot no intentará bloquear un 4 en raya del usuario si eso no le permite hacer la linea mas grande posible.
### Smart