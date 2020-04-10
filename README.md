# Cuatro en raya
Practica para la asignatura LP que sustituye el examen.

La practica consiste en implementar el juego de mesa Cuatro en raya junto con 3 bots para el juego. 
* El primero hace movimientos aleatorios.
* El segundo hace movimientos greedy evitando que el jugador gane si uno de sus posibles movimientos greedy evita una raya de 4 del jugador.
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
* Primero las dimensiones asegurate de no poner valores negativos o que al menos el valor de filas o de columnas sea mayor que 4.Justo despues se mostrar el tablero. Si alguna de las condiciones no se cumple, se mostrara un mensaje de error y volveran a pedirse las dimensiones

    ```
    >Elige el numero de filas del tablero
    6
    >Elige el numero de columnas del tablero
    7

    ["_","_","_","_","_","_","_"]
    ["_","_","_","_","_","_","_"]
    ["_","_","_","_","_","_","_"]
    ["_","_","_","_","_","_","_"]
    ["_","_","_","_","_","_","_"]
    ["_","_","_","_","_","_","_"]
    ```
* A continuacion se pedira la estrategia. Asegurate de poner uno de los valores mostrados o se mostrr un mensaje de error.
    ```
    >Que estrategia quieres que el bot tenga?: 1.Random 2.Greedy 3.Smart
    1
    ```
* Y finalmente se pedira quien comienza la partida, el bot escogido o el jugador.
    ```
    >Quieres tener el primer movimiento (escribe 0) o el segundo (escribe 1)?
    >Recuerda que el bot siempre sera las X y tu los O?
    0
    ```
## Como jugar
En este juego se escoge la columna en la que se quiere poner una ficha, y en cuanto se haya escogido esta se pondra en el fondo de la columna.
Las columnas estan numeradas desde el 1 hasta `n` siendo n el numero de columnas de la tabla.