import dash
import dash_bootstrap_components as dbc  # Importa Dash Bootstrap Components
from dash import Dash, html, Output, Input, State, no_update, dcc
from dash.exceptions import PreventUpdate
import os
import re
from flask import Flask

## Para hacer diccionario del mapa
archivos = os.listdir("assets/Datos/Mapas/") 
archivos_html = [f for f in archivos if f.endswith(".html")]

anios = [re.sub(r"\.html", "", i) for i in archivos_html]
anios = [re.sub(r"Mapa_", "", i) for i in anios]
anios = {i: anio for i, anio in enumerate(anios)}

archivos_html = [os.path.join("assets/Datos/Mapas/", f) for f in archivos_html]

######################################################
### Definición de los componentes de la aplicación ###
######################################################

slider_periodo = dcc.Slider(
    id="slider_periodo",
    step=None,
    marks=anios,
    value=list(anios.keys())[-1],
    className="slider-custom-R"
)

###############
### Modales ###
###############

modal_1 = dbc.Modal(
    [
        dbc.ModalHeader(
            dbc.ModalTitle("Explora el mapa")
        ),
        dbc.ModalBody(
            html.Div([
                html.P(
                    "Este mapa te permite ver los indicadores de calidad del agua en distintas fuentes de abastecimiento a lo largo de los años."
                ),
                html.Ol([
                    html.Li([
                        html.Strong("Línea de tiempo:"),
                        html.Ul([
                            html.Li("Puedes elegir el año que quieres ver.")
                        ])
                    ]),
                    html.Li([
                        html.Strong("Capas:"),
                        html.Ul([
                            html.Li("Selecciona diferentes indicadores de calidad del agua."),
                            html.Li([
                                "Verás las fuentes de abastecimiento coloreadas según su calidad en dicho indicador, basada en la norma ",
                                html.A("NOM-127-SSA1-2021", href="https://www.dof.gob.mx/nota_detalle_popup.php?codigo=5650705", target="_blank"),
                                "."
                            ]),
                            html.Li("Si una localidad tiene dos fuentes o más, se pintara la localidad apartir del promedio de ellas."),
                        ])
                    ]),
                    html.Li([
                        html.Strong("Buscador:"),
                        html.Ul([
                            html.Li("Busca una localidad, esta puede ser apartir del nombre de municipio y el mapa se centrará en ella.")
                        ])
                    ])
                ]),
                html.P("Cada parte del mapa está pensada para ayudarte a explorar la información de forma clara y visual.")
            ])
        ),
        dbc.ModalFooter(
            dbc.Button(
                "Ver información adicional",
                id="open-toggle-modal-2",
                className="ms-auto",
                n_clicks=0,
            )
        ),
    ],
    id="toggle-modal-1",
    is_open=False,
)

modal_2 = dbc.Modal(
    [
        dbc.ModalHeader(dbc.ModalTitle("Información Adicional")),
        dbc.ModalBody([
        "La Norma Oficial Mexicana ",
        html.A("NOM-127-SSA1-2021", href="https://www.dof.gob.mx/nota_detalle_popup.php?codigo=5650705", target="_blank"),
        " establece los límites permitidos de calidad del agua para uso y consumo humano. Estos límites están representados en cada símbologia del mapa.",
        ]),
        dbc.ModalFooter(
            dbc.Button(
                "Regresar a explorar el mapa",
                id="open-toggle-modal-1",
                className="ms-auto",
                n_clicks=0,
            )
        ),
    ],
    id="toggle-modal-2",
    is_open=False,
)

modal = html.Div(
    [
        dbc.Button("Open modal", id="open-toggle-modal", n_clicks=0),
        modal_1,
        modal_2,
    ]
)


###################################
### Estructura de la aplicación ###
###################################

encabezado =  dbc.Row([
        dbc.Col(
            html.H2("Indicadores de Calidad del Agua", style={'color': 'white', 'margin':'0', 'padding': '2vh 0 0 10px'}), # paddin arriba, derecha abajo izquierda
            width = 7,
            xxl = 7, xl = 7, lg = 7, md = 7, sm = 12,  xs = 12, 
            style = {'backgroundColor': '#9C2448', 'padding': '0','margin':'0'} 
        ),
        dbc.Col(
            #html.H2("Barra", style={'color': 'white', 'margin':'0', 'padding': '0'}),
            html.A(
                html.Img(src="./assets/Imagenes/Planeacion_dorado.png", style={'width': '100%', 'height': '70%', 'padding': '1vh 0 0 10px'}),
                href = "https://sigeh.hidalgo.gob.mx/", 
                target= "_blank"
            ),
            width = 3,
            xxl = 3, xl = 3, lg = 3, md = 3, sm = 7,  xs = 7, 
            style = {'backgroundColor': '#9C2448', 'padding': '0', 'margin':'0'}
        ),
        dbc.Col(
            html.A(
                html.Img(src="./assets/Imagenes/CEAA_dorado.png", style={'width': '75%', 'height': '75%', 'padding': '1vh 0 0 10px'}),
                href = "https://ceaa.hidalgo.gob.mx/",
                target= "_blank" 
            ),
            width = 2,
            xxl = 2, xl = 2, lg = 2, md = 2, sm = 5,  xs = 5, 
            style = {'backgroundColor': '#9C2448', 'padding': '0', 'margin':'0'}
        )
    ], 
    style={"height": "12vh", 'width': '100vw' , 'padding':'0', 'margin':'0'}
    )


enmedio = dbc.Row([
    dbc.Col(
        children= slider_periodo,
        width = 12,
        xxl = 12, xl = 12, lg = 12, md = 12, sm = 12,  xs = 12, 
        style = {'backgroundColor': '#BC955B', 'padding': '0','margin':'0'} 
    )
],
    style={"height": "8vh", 'width': '100vw' , 'padding':'0', 'margin':'0'}
)


mapa = dbc.Row(
    dbc.Col(
        html.Iframe(src= "", 
                    id="mapa",
                    style={'width': '100vw', 'height': '79vh', 'border': '0', 'padding': '0', 'margin': '0'}),
        width = 12,
        xxl = 12, xl = 12, lg = 12, md = 12, sm = 12,  xs = 12, 
        style = {'backgroundColor': '#9C2448', 'padding': '0','margin':'0'} 
    ),
    style={"height": "80vh", 'width': '100vw' , 'padding':'0', 'margin':'0'}
)



app = Dash(external_stylesheets=[dbc.themes.BOOTSTRAP])


app.layout = html.Div([
    encabezado,
    enmedio,
    mapa,
]
)


#################
### CALLBACKS ###
#################

@app.callback(
    Output("mapa", "src"),
    Input("slider_periodo", "value")
)
def actualizar_mapa(value):
    direccion_mapa = f"/assets/Datos/Mapas/Mapa_{anios[value]}.html"
    print(direccion_mapa)
    return direccion_mapa


if __name__ == '__main__':
    app.run(debug=True)
