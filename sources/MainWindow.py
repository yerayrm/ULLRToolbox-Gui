from PySide import QtCore, QtGui
from rpy2 import rinterface, robjects
from QTextEditCustom import QTextEditCustom
import sys

class MainWindow():

	# Constructor
	def __init__(self, ui, d_sobre):
		self.ui = ui
		self.d_sobre = d_sobre

		timer = QtCore.QTimer(self.ui)
		QtCore.QObject.connect(timer, QtCore.SIGNAL("timeout()"), self.update)
		timer.start(50)

		# toolbar
		self.setToolbar()

		# signals in main window
		QtCore.QObject.connect(self.ui.button_ejecutar, QtCore.SIGNAL("clicked()"), self.insertCommand)
		QtCore.QObject.connect(self.ui.button_clean, QtCore.SIGNAL("clicked()"), self.cleanConsole)
		QtCore.QObject.connect(self.ui.act_sobre, QtCore.SIGNAL("triggered()"), self.openSobre)

		# capture key event
		self.keyPressEater = QTextEditCustom(self.ui)
		self.ui.edit_comandos.installEventFilter(self.keyPressEater)

		# init methods
		self.initToolbox()
		self.initTable()



	def update(self):
		rinterface.process_revents()



	def setToolbar(self):
		# abrir archivo
		self.ui.act_abrir_archivo.setIcon(QtGui.QIcon('./resources/open.png'))
		self.ui.toolBar.addAction(self.ui.act_abrir_archivo)

		# guardar
		self.ui.act_guardar_archivo.setIcon(QtGui.QIcon('./resources/save.png'))
		self.ui.toolBar.addAction(self.ui.act_guardar_archivo)

		############# separador
		self.ui.toolBar.addSeparator()

		# nueva variable
		btn_variable = QtGui.QToolButton()
		btn_variable.setIcon(QtGui.QIcon('./resources/variable.png'))
		btn_variable.setMenu(self.ui.menuCrear_nueva)
		btn_variable.setPopupMode(QtGui.QToolButton.InstantPopup)
		self.ui.toolBar.addWidget(btn_variable)

		# agregado
		self.ui.act_agregado.setIcon(QtGui.QIcon('./resources/agregado.png'))
		self.ui.toolBar.addAction(self.ui.act_agregado)

		# segmentado
		self.ui.act_segmentado.setIcon(QtGui.QIcon('./resources/segmentado.png'))
		self.ui.toolBar.addAction(self.ui.act_segmentado)

		# extraer muestra
		self.ui.act_extraer_mues.setIcon(QtGui.QIcon('./resources/extraer_muestra.png'))
		self.ui.toolBar.addAction(self.ui.act_extraer_mues)

		# transformar
		self.ui.act_transformar.setIcon(QtGui.QIcon('./resources/transformar.png'))
		self.ui.toolBar.addAction(self.ui.act_transformar)

		############# separador
		self.ui.toolBar.addSeparator()

		# estadisticos descriptivos
		btn_variable = QtGui.QToolButton()
		btn_variable.setIcon(QtGui.QIcon('./resources/estadisticos_desc.png'))
		btn_variable.setMenu(self.ui.mnu_descriptiva)
		btn_variable.setPopupMode(QtGui.QToolButton.InstantPopup)
		self.ui.toolBar.addWidget(btn_variable)

		# contraste de medias
		btn_variable = QtGui.QToolButton()
		btn_variable.setIcon(QtGui.QIcon('./resources/contraste_medias.png'))
		btn_variable.setMenu(self.ui.mnu_contraste)
		btn_variable.setPopupMode(QtGui.QToolButton.InstantPopup)
		self.ui.toolBar.addWidget(btn_variable)

		# anova
		btn_variable = QtGui.QToolButton()
		btn_variable.setIcon(QtGui.QIcon('./resources/anova.png'))
		btn_variable.setMenu(self.ui.mnu_anova)
		btn_variable.setPopupMode(QtGui.QToolButton.InstantPopup)
		self.ui.toolBar.addWidget(btn_variable)

		############# separador
		self.ui.toolBar.addSeparator()

		# graficos
		btn_graficos = QtGui.QToolButton()
		btn_graficos.setIcon(QtGui.QIcon('./resources/graficas.png'))
		btn_graficos.setMenu(self.ui.mnu_graficos)
		btn_graficos.setPopupMode(QtGui.QToolButton.InstantPopup)
		self.ui.toolBar.addWidget(btn_graficos)



	# Initialize the ULLRToolbox script
	def initToolbox(self):
		def f(x):
			self.ui.text_result.textCursor().insertText(x)
		
		rinterface.set_writeconsole(f)
		backupList = robjects.globalenv.keys()
		resultado = robjects.r("source('./script/ULLRtoolbox.v.1.0.R')")
		currentList = robjects.globalenv.keys()
		
		if len(backupList) == len(currentList):
			self.ui.text_result.append(str(resultado))

		rinterface.set_writeconsole(rinterface.consolePrint)

	

	# Allow insert commands in a QTextEdit
	def insertCommand(self):
		comando = self.ui.edit_comandos.toPlainText()
		self.ui.text_result.append("> " + comando)

		def f(x):
			self.ui.text_result.textCursor().insertText(x)
		
		rinterface.set_writeconsole(f)
		backupList = robjects.globalenv.keys()
		resultado = robjects.r(comando)
		currentList = robjects.globalenv.keys()
		
		if len(backupList) == len(currentList):
			self.ui.text_result.append(str(resultado))

		rinterface.set_writeconsole(rinterface.consolePrint)


	def cleanConsole(self):
		self.ui.text_result.clear()


	# Initialize a QTableWidget when the program is open
	def initTable(self):
		nrow = 50
		ncol = 20
		
		self.ui.tableWidget.setRowCount(nrow)
		self.ui.tableWidget.setColumnCount(ncol)

		for i in range(ncol):
			ind = i+1
			header = "Var " + str(ind)
			newItem = QtGui.QTableWidgetItem(header)
			self.ui.tableWidget.setHorizontalHeaderItem(i, newItem)


	def openSobre(self):
		self.dialogUi = self.d_sobre
		self.dialogUi.setWindowTitle("Sobre ULLRToolbox")
		self.dialogUi.show()

