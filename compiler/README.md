# compiler

Compilador que traduce código en $\lambda\rho$ a Qiskit.

## Dependencias

- Instalar Stack (https://docs.haskellstack.org/en/stable/README/), Python y GHC.
- Instalar librerías de Python:
```
pip install -r requirements.txt
```

## Cómo ejecutar

Algunos comandos posibles:
```
stack build # Compilar
stack run -- -i # Consola interactiva de Lambda Rho
stack run -- -f example.rho  -o example.py && python example.py  # Entrada salida de archivos
stack ghci # Consola de Haskell con módulos cargados
```


## Testing

Para testear:
```
stack test
```


Para testear un prefijo y activar los dprints:
```
stack test --ghc-options="-DDEBUG" --test-arguments="-p tests.pythonTests.gateTests.lambda"  
```

Para calcular el cubrimiento (borra los dprints primero):
```
find src -type f -name '*.hs' -exec sed -i 's/dprint "[a-zA-Z]*" \$ //g' {} \; && stack test --coverage
```
