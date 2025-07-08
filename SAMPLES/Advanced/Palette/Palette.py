from tkinter import Tk, Frame, Entry

window = Tk()
window.title("Palette")
window.geometry("600x400")

frame = Frame(window, bd=5, relief="solid")
frame.place(x=10, y=10, width=580, height=200)

entry = Entry(window, bd=5, relief="solid", font=("Martian Mono", 20))
entry.place(x=10, y=320, width=580, height=50)

while True:
    window.update()

    try:
        frame.configure(bg=entry.get())
    except:
        pass
