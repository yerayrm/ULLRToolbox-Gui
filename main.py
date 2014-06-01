#!/usr/bin/env python
# -*- coding: utf-8 -*-

from PySide import QtCore, QtGui
from PySide.QtUiTools import QUiLoader
from PySide.QtGui import QApplication, QLineEdit

from sources.MainWindow import MainWindow
from sources.FncArchivo import FncArchivo
from sources.FncDatosAgregado import FncDatosAgregado
from sources.FncDatosSegmentado import FncDatosSegmentado
from sources.FncDatosCrear import FncDatosCrear
from sources.FncDatosDiscretizar import FncDatosDiscretizar
from sources.FncDatosTipificar import FncDatosTipificar
from sources.FncDatosRecodificar import FncDatosRecodificar
from sources.FncDatosMuestraTransformar import FncDatosMuestraTransformar
from sources.FncGrafCajasFrec import FncGrafCajasFrec
from sources.FncGrafDispersion import FncGrafDispersion
from sources.FncGrafPercenHisto import FncGrafPercenHisto
from sources.FncAnlsDescriptivos import FncAnlsDescriptivos
from sources.FncAnlsContraste import FncAnlsContraste
from sources.FncAnlsCorrelacion import FncAnlsCorrelacion
from sources.FncAnlsAnova import FncAnlsAnova
from sources.FncAnlsRegresion import FncAnlsRegresion
from sources.QTextEditCustom import QTextEditCustom

import rpy2.rinterface as rinterface
import time
import threading

import sys
import resource_rc

resource_rc.qInitResources()

def main(argv=None):
    app = QApplication(sys.argv)
    loader = QUiLoader()

    # loader del mainwindows y todos los dialogs
    ui                  = loader.load(':/mainwindow')
    d_agregado          = loader.load(':/agregado_dialog')
    d_segmentado        = loader.load(':/segmentado_dialog')
    d_crearVar_0        = loader.load(':/crear_variable_existente')
    d_crearVar_1        = loader.load(':/crear_variable_funcion')
    d_crearVar_2        = loader.load(':/crear_variable_algoritmo')
    d_multiselector     = loader.load(':/multiselect_column')
    d_multiselector_2   = loader.load(':/multiselect_column')
    d_discretizar       = loader.load(':/discretizar_dialog')
    d_tipificar         = loader.load(':/tipificar_dialog')
    d_recodificar       = loader.load(':/recodificar_dialog')
    d_muestra           = loader.load(':/muestra_dialog')
    d_transformar       = loader.load(':/transformar_dialog')
    d_cajas             = loader.load(':/cajas_dialog')
    d_frec              = loader.load(':/frecuencias_dialog')
    d_dispersion        = loader.load(':/dispersion_dialog')
    d_percentiles       = loader.load(':/percentiles_dialog')
    d_histogramas       = loader.load(':/histograma_dialog')
    d_descr_univar      = loader.load(':/descr_univar_dialog')
    d_descr_multiple    = loader.load(':/descr_multiple_dialog')
    d_correlacion       = loader.load(':/correlacion_dialog')
    d_contr_inter       = loader.load(':/contr_inter_dialog')
    d_contr_intra       = loader.load(':/contr_intra_dialog')
    d_anova_inter       = loader.load(':/anova_inter_dialog')
    d_anova_intra       = loader.load(':/anova_intra_dialog')
    d_anova_split       = loader.load(':/anova_split_dialog')
    d_regr_multiple     = loader.load(':/regr_multiple_dialog')
    d_regr_mediacion    = loader.load(':/regr_mediacion_dialog')

    # instancio el mainwindow
    mWindow             = MainWindow(ui)
    fncArchivo          = FncArchivo(ui)
    fndDatosAgregado    = FncDatosAgregado(ui, d_agregado)
    fncDatosSegmentado  = FncDatosSegmentado(ui, d_segmentado)
    fncDatosCrear       = FncDatosCrear(ui, d_multiselector, d_crearVar_0, d_crearVar_1, d_crearVar_2)
    fncDatosDiscretizar = FncDatosDiscretizar(ui, d_discretizar)
    fncDatosTipificar   = FncDatosTipificar(ui, d_tipificar, d_multiselector)
    fncDatosRecodificar = FncDatosRecodificar(ui, d_recodificar)
    fncDatosMuesTra     = FncDatosMuestraTransformar(ui, d_muestra, d_transformar)
    fncGrafCajasFrec    = FncGrafCajasFrec(ui, d_cajas, d_frec, d_multiselector)
    fncGrafDispersion   = FncGrafDispersion(ui, d_dispersion, d_multiselector)
    fncGrafPercenHisto  = FncGrafPercenHisto(ui, d_percentiles, d_histogramas, d_multiselector)
    fncAnlsDescriptivos = FncAnlsDescriptivos(ui, d_descr_univar, d_descr_multiple, d_multiselector)
    fncAnlsCorrelacion  = FncAnlsCorrelacion(ui, d_correlacion, d_multiselector, d_multiselector_2)
    fncAnlsContraste    = FncAnlsContraste(ui, d_contr_inter, d_contr_intra, d_multiselector)
    fncAnlsAnova        = FncAnlsAnova(ui, d_anova_inter, d_anova_intra, d_anova_split, d_multiselector)
    fncAnlsRegresion    = FncAnlsRegresion(ui, d_regr_multiple, d_regr_mediacion, d_multiselector)

    # inicio el programa
    ui.setWindowTitle("ULLRToolbox")
    ui.setWindowIcon(QtGui.QIcon(':/logo64'))
    ui.showMaximized()

    return app.exec_()




if __name__ == '__main__':
    main()    