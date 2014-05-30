#!/usr/bin/env python
# -*- coding: utf-8 -*-

from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncAnlsAnova():

	#Constructor
	def __init__(self, ui, d_anova_inter, d_anova_intra, d_anova_split):
		self.ui = ui
		self.d_anova_inter = d_anova_inter
		self.d_anova_intra = d_anova_intra
		self.d_anova_split = d_anova_split

		QtCore.QObject.connect(self.ui.act_anv_intergrupo, QtCore.SIGNAL("triggered()"), self.openAnovaInter)
		QtCore.QObject.connect(self.ui.act_anv_intragrupo, QtCore.SIGNAL("triggered()"), self.openAnovaIntra)
		QtCore.QObject.connect(self.ui.act_anv_splitplot, QtCore.SIGNAL("triggered()"), self.openAnovaSplit)
		


	def openAnovaInter(self):
		self.dialogUi = self.d_anova_inter
		self.dialogUi.setWindowTitle("ANOVA intergrupo".decode("utf8"))
		self.dialogUi.show()

	def openAnovaIntra(self):
		self.dialogUi = self.d_anova_intra
		self.dialogUi.setWindowTitle("ANOVA intragrupo".decode("utf8"))
		self.dialogUi.show()

	def openAnovaSplit(self):
		self.dialogUi = self.d_anova_split
		self.dialogUi.setWindowTitle("ANOVA split-plot".decode("utf8"))
		self.dialogUi.show()