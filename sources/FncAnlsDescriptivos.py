#!/usr/bin/env python
# -*- coding: 850 -*-
# -*- coding: utf-8 -*-

from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncAnlsDescriptivos():

	#Constructor
	def __init__(self, ui, d_descr_univar, d_descr_multiple):
		self.ui = ui
		self.d_descr_univar = d_descr_univar
		self.d_descr_multiple = d_descr_multiple

		QtCore.QObject.connect(self.ui.act_descr_univar, QtCore.SIGNAL("triggered()"), self.openDescrUnivar)
		QtCore.QObject.connect(self.ui.act_descr_multiple, QtCore.SIGNAL("triggered()"), self.openDescrMultiple)
		


	def openDescrUnivar(self):
		self.dialogUi = self.d_descr_univar
		self.dialogUi.setWindowTitle(("Estadísticos descriptivos con variable dependiente").decode("utf8"))
		self.dialogUi.show()


	def openDescrMultiple(self):
		self.dialogUi = self.d_descr_multiple
		self.dialogUi.setWindowTitle(("Estadísticos descriptivos con multiples variables").decode("utf8"))
		self.dialogUi.show()