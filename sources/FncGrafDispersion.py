from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
import sys

class FncGrafDispersion():

	#Constructor
	def __init__(self, ui, d_dispersion, d_multiselector):
		self.ui = ui
		self.d_dispersion = d_dispersion
		self.d_multiselector = d_multiselector

		QtCore.QObject.connect(self.ui.act_dispersion, QtCore.SIGNAL("triggered()"), self.openDispersion)
		


	def openDispersion(self):
		self.dialogUi = self.d_dispersion
		self.dialogUi.setWindowTitle("Grafica de dispersion")
		self.dialogUi.show()

		# Abro el multiselector
		self.dialogUi.var_select.clear()
		QtCore.QObject.connect(self.dialogUi.pushMultiSelector, QtCore.SIGNAL("clicked()"), self.openMultiSelector, QtCore.Qt.UniqueConnection)

		# Inicializo los combobox
		def f(x):
			print x
		rinterface.set_writeconsole(f)
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]
		self.dialogUi.cb_factor.clear()
		self.dialogUi.cb_factor.addItem(str("Sin factor"))
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogUi.cb_factor.addItem(str(item_factor[0]))

		rinterface.set_writeconsole(rinterface.consolePrint)

		# Signals
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("accepted()"), self.accept, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogUi.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def accept(self):
		print ("*Accept*")
		variables = "c('"
		variables = variables + "', '".join(self.listMulti)
		variables = variables + "')"
		
		factor = str(self.dialogUi.cb_factor.currentText())

		if (factor == "Sin factor"):
			comando = "grafica.dispersion.fnc(datos, variables=" + variables + ")"
		else:
			comando = "grafica.dispersion.fnc(datos, variables=" + variables + ", que.factor='" + factor + "')"

		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x)

		rinterface.set_writeconsole(f)
		resultado = robjects.r(comando)
		self.ui.text_result.append(str(resultado))
		
		rinterface.set_writeconsole(rinterface.consolePrint)



	def cancel(self):
		print ("*Cancel*")



	def openMultiSelector(self):
		self.dialogMS = self.d_multiselector
		self.dialogMS.setWindowTitle("Selecciona los parametros")
		self.dialogMS.show()
		n_items = "length(names(datos))"
		n_items = robjects.r(n_items)
		n_items = n_items[0]

		self.dialogMS.selector_left.clear()
		self.dialogMS.selector_right.clear()
		for i in range(n_items):
			item_factor = "names(datos)[" + str(i+1) + "]"
			item_factor = robjects.r(item_factor)
			self.dialogMS.selector_left.insertItem(i, str(item_factor[0]))

		QtCore.QObject.connect(self.dialogMS.moveToLeft, QtCore.SIGNAL("clicked()"), self.moveToLeft, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogMS.moveToRight, QtCore.SIGNAL("clicked()"), self.moveToRight, QtCore.Qt.UniqueConnection)

		QtCore.QObject.connect(self.dialogMS.buttonBox, QtCore.SIGNAL("accepted()"), self.acceptMultiSelector, QtCore.Qt.UniqueConnection)
		QtCore.QObject.connect(self.dialogMS.buttonBox, QtCore.SIGNAL("rejected()"), self.cancel, QtCore.Qt.UniqueConnection)



	def moveToRight(self):
		self.dialogMS.selector_right.addItem(str(self.dialogMS.selector_left.currentItem().text()))
		self.dialogMS.selector_left.takeItem(self.dialogMS.selector_left.currentRow())
		print "right n=" + str(self.dialogMS.selector_right.count())
		print "left n=" + str(self.dialogMS.selector_left.count())



	def moveToLeft(self):
		self.dialogMS.selector_left.addItem(str(self.dialogMS.selector_right.currentItem().text()))
		self.dialogMS.selector_right.takeItem(self.dialogMS.selector_right.currentRow())
		print "right n=" + str(self.dialogMS.selector_right.count())
		print "left n=" + str(self.dialogMS.selector_left.count())



	def acceptMultiSelector(self):
		self.listMulti = []
		if (self.dialogMS.selector_right.count() > 0):
			for i in range(self.dialogMS.selector_right.count()):
				self.listMulti.append(self.dialogMS.selector_right.item(i).text())

		self.dialogUi.var_select.clear()
		self.dialogUi.var_select.insert(', '.join(self.listMulti))
		print ', '.join(self.listMulti)


