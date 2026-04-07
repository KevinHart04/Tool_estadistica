#!/usr/bin/env python3
"""
Módulo principal de Statify.
Provee una Interfaz de Usuario de Terminal (TUI) para gestionar
análisis estadísticos interactuando con scripts de R en el backend.
"""

import sys
import subprocess
import questionary
from rich.console import Console
from rich.panel import Panel
from rich.text import Text
import pyfiglet

console = Console()

def imprimir_banner(texto: str = "Statify", fuente: str = "standard", color: str = "blue1") -> None:
    """
    Genera e imprime un banner en ASCII Art estilizado usando pyfiglet y rich.
    
    Args:
        texto (str): La palabra a convertir en ASCII Art.
        fuente (str): El nombre de la tipografía de pyfiglet. 
        color (str): El color del texto compatible con rich.
    """
    ascii_art = pyfiglet.figlet_format(texto, font=fuente)
    panel = Panel(
        Text(ascii_art, style=f"bold {color}", justify="center"),
        border_style=color,
        expand=False,
        subtitle="[italic]Análisis Estadístico TUI[/italic]"
    )
    console.print(panel)

def mostrar_error(mensaje: str) -> None:
    """
    Imprime un mensaje de error con un formato distintivo.
    
    Args:
        mensaje (str): El texto del error a mostrar.
    """
    console.print(f"\n[bold white on red] ERROR [/] [red]{mensaje}[/]\n")

def mostrar_exito(mensaje: str) -> None:
    """
    Imprime un mensaje de éxito.
    
    Args:
        mensaje (str): El texto de confirmación a mostrar.
    """
    console.print(f"\n[bold green]✓ {mensaje}[/]\n")

def limpiar_ruta(ruta: str) -> str:
    """
    Limpia la ruta del archivo ingresada mediante arrastre (Drag & Drop).
    
    Args:
        ruta (str): La ruta cruda capturada del input.
        
    Returns:
        str: La ruta limpia sin comillas ni espacios extra.
    """
    return ruta.strip(" '\"")

def menu_graficos(es_continua: bool) -> list:
    """
    Despliega un submenú interactivo para seleccionar gráficos.
    
    Args:
        es_continua (bool): True si la variable es continua.
        
    Returns:
        list: Identificadores de los gráficos seleccionados.
    """
    graficos_continuos = [
        {"name": "Histograma", "value": "histograma"},
        {"name": "Polígono de Frecuencias", "value": "poligono"},
        {"name": "Ojiva", "value": "ojiva"}
    ]
    
    graficos_discretos = [
        {"name": "Gráfico de Barras", "value": "barras"},
        {"name": "Gráfico de Sectores", "value": "sectores"},
        {"name": "Gráfico Escalonado", "value": "escalonado"}
    ]
    
    opciones = graficos_continuos if es_continua else graficos_discretos
    opciones.append({"name": "Generar Todos (all)", "value": "all"})
    
    seleccion = questionary.checkbox(
        "Selecciona los gráficos (Espacio para marcar, Enter para confirmar):",
        choices=opciones
    ).ask()
    
    if "all" in seleccion:
        return ["all"]
    return seleccion

def ejecutar_r(estado: dict, centralizacion: bool = False, ejecutar_graficos: bool = False) -> None:
    """
    Construye y despacha el subproceso que invoca al script principal de R.
    
    Args:
        estado (dict): Configuración actual (archivo, variable, tipo).
        centralizacion (bool): True para mostrar solo medidas centrales.
        ejecutar_graficos (bool): True para adjuntar flags de gráficos.
    """
    if not estado["archivo"] or not estado["variable"]:
        mostrar_error("Debes cargar un archivo y definir la variable primero (Opciones 1 y 2).")
        return

    comando = ["Rscript", "main.R", "-b", estado["archivo"], "-v", estado["variable"], "-t", estado["tipo"]]
    
    if centralizacion:
        comando.append("--centralizacion")
    elif ejecutar_graficos and estado["graficos"]:
        comando.extend(["-g", ",".join(estado["graficos"])])
            
    console.print("\n[bold black on white] Ejecutando Motor R... [/]\n")
    try:
        subprocess.run(comando, check=True)
        mostrar_exito("Procesamiento finalizado.")
    except subprocess.CalledProcessError:
        mostrar_error("El motor de R falló. Verifica el nombre de la variable y el archivo.")
    except FileNotFoundError:
        mostrar_error("No se encontró 'Rscript'. Verifica que R esté instalado en el sistema.")

def loop_interactivo() -> None:
    """
    Bucle principal interactivo. Mantiene el estado de la sesión.
    """
    estado = {"archivo": None, "variable": None, "tipo": "discreta", "graficos": []}
    imprimir_banner()
    
    while True:
        opcion = questionary.select(
            "Menú Principal:",
            choices=[
                f"1. Cargar archivo (Actual: {estado['archivo'] or 'Ninguno'})",
                f"2. Elegir variable y tipo (Actual: {estado['variable']} - {estado['tipo']})",
                "3. Generar Distribución de Frecuencias (Tabla)",
                "4. Seleccionar y Generar Gráficos",
                "5. Medidas de Centralización e Interpretación",
                "6. Salir"
            ]
        ).ask()

        if opcion is None or opcion.startswith("6"):
            console.print("\n[bold orange1]Saliendo de Statify. ¡Nos vemos![/]\n")
            break
            
        elif opcion.startswith("1"):
            ruta = console.input("\n[cyan]Arrastra el archivo de datos aquí y presiona Enter:[/]\n> ")
            estado["archivo"] = limpiar_ruta(ruta)
            
        elif opcion.startswith("2"):
            estado["variable"] = questionary.text("Escribe el nombre EXACTO de la variable a procesar:").ask()
            estado["tipo"] = questionary.select(
                "Selecciona la naturaleza de la variable:",
                choices=["discreta", "continua"]
            ).ask()
            estado["graficos"] = [] 
            
        elif opcion.startswith("3"):
            ejecutar_r(estado)
            
        elif opcion.startswith("4"):
            if not estado["variable"]:
                mostrar_error("Define la variable en el paso 2 antes de graficar.")
                continue
            seleccionados = menu_graficos(estado["tipo"] == "continua")
            if seleccionados:
                estado["graficos"] = seleccionados
                ejecutar_r(estado, ejecutar_graficos=True)
            
        elif opcion.startswith("5"):
            ejecutar_r(estado, centralizacion=True)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        subprocess.run(["Rscript", "main.R"] + sys.argv[1:])
    else:
        loop_interactivo()