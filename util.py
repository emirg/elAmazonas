#Ignorar, lo use para modificar el archivo
#y no tener que estar dos horas escribiendo lo mismo
def main():
	f= open("juego.pl","r")
	nuevo = open("juego.pl.tmp","w+")
	f1 = f.readlines()
	y = 1
	for x in f1:
		if y >60 and y<733:
			if x != "\n" and x != "%":
				nuevo.write(x)
		y += 1		



if __name__== "__main__":
	main()