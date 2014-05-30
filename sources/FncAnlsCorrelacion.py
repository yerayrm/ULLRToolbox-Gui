#!/usr/bin/env python
# -*- coding: utf-8 -*-

from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncAnlsCorrelacion():

	#Constructor
	def __init__(self, ui, d_correlacion):
		self.ui = ui
		self.d_correlacion = d_correlacion

		QtCore.QObject.connect(self.ui.act_correlacion, QtCore.SIGNAL("triggered()"), self.openCorrelacion)
		


	def openCorrelacion(self):
		self.dialogUi = self.d_correlacion
		self.dialogUi.setWindowTitle("Correlación".decode("utf8"))
		self.dialogUi.show()