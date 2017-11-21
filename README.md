# Retina Interpreter

## Retina
[Logo] inspired language, based on image generation. Each program has a 'pen' and commands to draw with it.

## Interpreter
Made with Haskell for a project [Translators and Interpreters] course at Universidad Simon Bolivar.

## Examples:
```
program
    for k from 1 to 10 do
        for i from 1 to 50 do
            forward(1);
            rotatel(1);
        end;
        for i from 1 to 90 do
            forward(1);
            rotater(1);
        end;
    end;
end;
```
![Imgur](https://i.imgur.com/fNxmbII.png)



```
program
    with
    do
        for i from 1 to 100 do
            forward(i * 2); # Traza una línea por 50 puntos
            rotater(90); # Gira 90 grados sentido horario
        end;
    end;
end;
```
![Imgur](https://i.imgur.com/emySYZI.png)



```
program
    with
        number l=100;
        number nvertex = 5;
    do
        rotatel(1);
        for i from 1 to nvertex do
            forward(l);
            rotatel(70);
            forward(l);
            rotater(70+360/nvertex);
        end;
    end;
end;
```
![Imgur](https://i.imgur.com/6PXwiqF.png)



```
program
    for k from 3 to 12 do
        for i from 1 to k do
            forward(60);
            rotatel(360/k);
        end;
    end;
end;
```
![Imgur](https://i.imgur.com/07xfa9K.png)



```
program
    with
        number l=110, rep=360;
        number nvertex = 5;
    do
        repeat rep times
            rotater(1);
            for i from 1 to nvertex do
                forward(l);
                rotatel(80);
                forward(l);
                rotater(80+360/nvertex);
            end;
        end;
    end;
end;
```
![Imgur](https://i.imgur.com/z3pxEb5.png)



```
program
    for i from 1 to 17 do
        forward(350);
        rotater(360*8/17);
    end;
end;
```
![Imgur](https://i.imgur.com/biH5Czc.png)


[//]: # (References)

[Logo]: <http://el.media.mit.edu/logo-foundation/what_is_logo/logo_programming.html>
[Translators and Interpreters] : <http://ldc.usb.ve/~emhn/cursos/ci3725/>
