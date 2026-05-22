# textos.py
#
# Este es nuestro primer script de python
# para utilizar como aplicacion independiente

def eliminarVocales(texto):
    return ''.join([c for c in texto if c.lower() not in 'aeiou'])

def contarPalabras(texto):
    return len(texto.split())