from dash import Dash, html
import dash_bootstrap_components as dbc
from dash import Input, Output, State, html



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


app = Dash(external_stylesheets=[dbc.themes.BOOTSTRAP])
app.layout = html.Div(
    [
        modal,
    ]
)


@app.callback(
    Output("toggle-modal-1", "is_open"),
    [
        Input("open-toggle-modal", "n_clicks"),
        Input("open-toggle-modal-1", "n_clicks"),
        Input("open-toggle-modal-2", "n_clicks"),
    ],
    [State("toggle-modal-1", "is_open")],
)
def toggle_modal_1(n0, n1, n2, is_open):
    if n0 or n1 or n2:
        return not is_open
    return is_open


@app.callback(
    Output("toggle-modal-2", "is_open"),
    [
        Input("open-toggle-modal-2", "n_clicks"),
        Input("open-toggle-modal-1", "n_clicks"),
    ],
    [State("toggle-modal-2", "is_open")],
)
def toggle_modal_2(n2, n1, is_open):
    if n1 or n2:
        return not is_open
    return is_open






if __name__ == '__main__':
    app.run(debug=True)