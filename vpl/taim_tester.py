from edutest import *
from progkontroll import *
from inspect import signature
import ast

def õige_vastus(n):
    P1 = 1
    P2 = P3 = K = 0
    for i in range(n):
        P3eel = P3
        P2eel = P2
        P1eel = P1
        Keel = K
        P3 = P2eel
        P2 = P1eel
        P1 = Keel
        K = Keel + P1eel + P2eel
    return P3

def check(n):
    
    programminimi = "taim.py"
    
    progolemasolu(programminimi)
                
    with Plan("Testin programmi sisendiga n={}.".format(n)):
        
        # Programmi käivitamine

        sisendid = [n] * 20
        oodatav_sisendite_arv = 1
        veatekst = "Programmi ainus sisend peaks olema nädala number."

        globs, cap = täidaprog(programminimi, sisendid, oodatav_sisendite_arv, veatekst)
                
        # Vastuse kontrollimine
        
        väljund = cap.get_last_stdout()        
        tegelik_arvud = list(filter(on_täisarv, re.findall(r'\b[0-9.]+\b', väljund)))
        
        # Kas vastuses leidub arve
        
        if len(tegelik_arvud) == 0:
            teade = "Ei leidnud väljundist ühtegi arvu.\n"
            teade += "\nProgrammi väljund oli:" + quote_text_block(väljund)
            lõpeta(teade)
            
        if len(tegelik_arvud) > 1:
            teade = "Leidsin programmi väljundist mitu arvu: " + jadasõne(tegelik_arvud) + ".\n"
            teade += "Milline neist on vastus?\n"
            teade += "\nProgrammi väljund oli:" + quote_text_block(väljund)
            lõpeta(teade)
            
        # Kas vastus on õige
                
        oodatav_tulemus = õige_vastus(n)
        tegelik_tulemus = int(tegelik_arvud[0])
        
        if tegelik_tulemus != oodatav_tulemus:
            teade = "Vastus " + str(tegelik_tulemus) + " ei ole õige.\n"
            teade += "\nProgrammi väljund oli:" + quote_text_block(väljund)
            lõpeta(teade)
            
def test1(): check(1)
def test2(): check(2)
def test3(): check(3)
def test4(): check(4)
def test5(): check(7)
def test6(): check(9)
def test7(): check(12)
def test8(): check(17)
def test9(): check(22)
def test10(): check(23)
def test11(): check(26)
def test12(): check(30)
def test13(): check(35)
def test14(): check(40)
def test15(): check(41)
def test16(): check(42)
def test17(): check(65)
def test18(): check(66)
def test19(): check(69)
def test20(): check(75)

#test1()
#test2()
#test3()
#test4()
#test5()
#test6()
#test7()
#test8()
#test9()
#test10()
#test11()
#test12()
#test13()
#test14()
#test15()
#test16()
#test17()
#test18()
#test19()
#test20()
