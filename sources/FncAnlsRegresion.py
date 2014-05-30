#!/usr/bin/env python
# -*- coding: utf-8 -*-

from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncAnlsRegresion():

	#Constructor
	def __init__(self, ui, d_regr_multiple, d_regr_mediacion):
		self.ui = ui
		self.d_regr_multiple = d_regr_multiple
		self.d_regr_mediacion = d_regr_mediacion

		QtCore.QObject.connect(self.ui.act_regr_multiple, QtCore.SIGNAL("triggered()"), self.openRegresionMultiple)
		QtCore.QObject.connect(self.ui.act_regr_mediacion, QtCore.SIGNAL("triggered()"), self.openMediacion)
		


	def openRegresionMultiple(self):
		self.dialogUi = self.d_regr_multiple
		self.dialogUi.setWindowTitle(("Regresión múltiple").decode("utf8"))
		self.dialogUi.show()

	def openMediacion(self):
		self.dialogUi = self.d_regr_mediacion
		self.dialogUi.setWindowTitle(("Mediación").decode("utf8"))
		self.dialogUi.show()