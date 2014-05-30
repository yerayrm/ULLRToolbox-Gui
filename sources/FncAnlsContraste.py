#!/usr/bin/env python
# -*- coding: utf-8 -*-

from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncAnlsContraste():

	#Constructor
	def __init__(self, ui, d_contr_inter, d_contr_intra):
		self.ui = ui
		self.d_contr_inter = d_contr_inter
		self.d_contr_intra = d_contr_intra

		QtCore.QObject.connect(self.ui.act_contr_inter, QtCore.SIGNAL("triggered()"), self.openContrInter)
		QtCore.QObject.connect(self.ui.act_contr_intra, QtCore.SIGNAL("triggered()"), self.openContrIntra)



	def openContrInter(self):
		self.dialogUi = self.d_contr_inter
		self.dialogUi.setWindowTitle(("Contraste T intergrupo").decode("utf8"))
		self.dialogUi.show()


	def openContrIntra(self):
		self.dialogUi = self.d_contr_intra
		self.dialogUi.setWindowTitle(("Contraste T intragrupo").decode("utf8"))
		self.dialogUi.show()