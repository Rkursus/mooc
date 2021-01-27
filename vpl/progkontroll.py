from edutest import *
from inspect import signature
import ast
import os
from copy import deepcopy

def lõpeta(teade, väljund = ""):
    if väljund != "":
        if teade[-1] != '\n':
            teade += '\n'
        teade += "\nVäljund oli:" + quote_text_block(väljund)        
    raise AssertionError(teade)

def arvsõne(n, ainsus, mitmus):
    s = str(n) + " "
    if n == 1:
        s += ainsus
    else:
        s += mitmus
    return s

def jadasõne(a):
    s = ""
    for i in range(len(a)):
        if i > 0:
            s += " "            
        s += str(a[i])
        if i < len(a) - 2:
            s += ","
        elif i == len(a) - 2:
            s += " ja"
    return s

def on_arv(s):
    try:
        float(str(s))
        return True
    except ValueError:
        return False
    except TypeError:
        return False
    
def on_täisarv(s):    
    try:
        int(str(s))        
        return True
    except ValueError:
        return False
    except TypeError:
        return False
    
def progolemasolu(programminimi):
    assert os.path.isfile(programminimi), "Ei leia faili %s." % programminimi
    assert os.path.getsize(programminimi) > 0, "Failil %s puudub sisu." % programminimi

def progsisendiarvukontroll(tegelik_sisendite_arv, oodatav_sisendite_arv, veatekst=""):
    if tegelik_sisendite_arv != oodatav_sisendite_arv:
        if tegelik_sisendite_arv == 0 and oodatav_sisendite_arv > 0:
            teade = "Programm ei küsinud sisendeid.\n"
            teade += "Programm peaks saama sisendi kasutaja käest."
            lõpeta(teade)
        elif tegelik_sisendite_arv > 0 and oodatav_sisendite_arv == 0:
            teade = "Programm küsis sisendeid.\n"
            teade += "Programm ei tohiks üldse sisendeid küsida."
            lõpeta(teade)
        elif tegelik_sisendite_arv > 0 and oodatav_sisendite_arv > 0:
            teade = "Programm küsis "
            teade += ("vähem" if tegelik_sisendite_arv < oodatav_sisendite_arv else "rohkem")
            teade += " sisendeid kui oleks pidanud.\n"
            teade += "Programm küsis " + arvsõne(tegelik_sisendite_arv, "sisendit", "sisendit") \
                     + " aga oleks pidanud küsima " + arvsõne(oodatav_sisendite_arv, "sisendit", "sisendit") + ".\n"
            teade += veatekst
            lõpeta(teade)
            
def täidaprog(programminimi, sisendid, oodatav_sisendite_arv, veatekst=""):
    globs, cap = run_script(programminimi, sisendid)
    tegelik_sisendite_arv = len(sisendid) - len(cap.get_remaining_inputs())
    progsisendiarvukontroll(tegelik_sisendite_arv, oodatav_sisendite_arv, veatekst)
    return globs, cap

def funksignatuur(globs, funktsiooninimi, oodatav_parameetrite_arv, veatekst=""):
    if funktsiooninimi not in globs:
        lõpeta("Ei leia programmist funktsiooni '" + str(funktsiooninimi) + "'.")
        
    tegelik_parameetrite_arv = len(signature(globs[funktsiooninimi]).parameters)    
    if tegelik_parameetrite_arv != oodatav_parameetrite_arv:
        teade = "Funktsioonil '" + str(funktsiooninimi) + "' on " + \
                arvsõne(tegelik_parameetrite_arv, "parameeter", "parameetrit") + ", "\
                "aga peaks olema " + \
                arvsõne(oodatav_parameetrite_arv, "parameeter", "parameetrit") + "."
        # Parameetrite kirjeldus
        if veatekst != "":
            teade += "\n" + veatekst
        lõpeta(teade)

def funksisendiarvukontroll(funktsiooninimi, tegelik_sisendite_arv, oodatav_sisendite_arv, veatekst=""):
    if tegelik_sisendite_arv != oodatav_sisendite_arv:
        if tegelik_sisendite_arv == 0 and oodatav_sisendite_arv > 0:
            teade = "Funktsioon '" + str(funktsiooninimi) + "' ei küsinud sisendeid.\n"
            teade += "Funktsioon peaks sisendi saama kasutaja käest."
            lõpeta(teade)
        elif tegelik_sisendite_arv > 0 and oodatav_sisendite_arv == 0:
            teade = "Funktsioon '" + str(funktsiooninimi) + "' küsis "
            teade += ("sisendit" if tegelik_sisendite_arv == 1 else "sisendeid") + ".\n"
            teade += "Funktsioon ei peaks sisendeit küsima, andmed tuleks talle anda argumentide kaudu."
            lõpeta(teade)
        elif tegelik_sisendite_arv > 0 and oodatav_sisendite_arv > 0:
            teade = "Funktsioon '" + str(funktsiooninimi) + "' küsis "
            teade += ("vähem" if tegelik_sisendite_arv < oodatav_sisendite_arv else "rohkem")
            teade += " sisendeid kui oleks pidanud.\n"
            teade += "Funktsioon küsis " + arvsõne(tegelik_sisendite_arv, "sisendit", "sisendit") \
                     + " aga oleks pidanud küsima " + arvsõne(oodatav_sisendite_arv, "sisendit", "sisendit") + ".\n"
            teade += veatekst
            lõpeta(teade)

def täidafunk(funktsioon, argumendid, sisendid, oodatav_sisendite_arv, veatekst=""):
    with IOCapturing(sisendid) as cap:
        tegelik_tulemus = funktsioon(*argumendid)
    tegelik_sisendite_arv = len(sisendid) - len(cap.get_remaining_inputs())
    funksisendiarvukontroll(funktsioon.__name__, tegelik_sisendite_arv, oodatav_sisendite_arv, veatekst)
    return tegelik_tulemus, cap

def progpöördub(programminimi, funktsiooninimi):
    with open(programminimi, encoding='utf-8') as f:
        programmisisu = f.read()
        süntaksipuu = ast.parse(programmisisu)
        if type(funktsiooninimi) == str:
            kontrollitavad = [funktsiooninimi]
        elif type(funktsiooninimi) == list:
            kontrollitavad = funktsiooninimi            
        for tipp in ast.walk(süntaksipuu):
            if isinstance(tipp, ast.Call):
                if 'func' in tipp._fields:
                    if 'id' in tipp.func._fields:
                        for i in range(len(kontrollitavad)):
                            if tipp.func.id == kontrollitavad[i]:
                                return True
                    if 'attr' in tipp.func._fields:
                        for i in range(len(kontrollitavad)):
                            if tipp.func.attr == kontrollitavad[i]:
                                return True
    return False
        
def funkpöördub(programminimi, funktsiooninimi, kontrollitavnimi):
    with open(programminimi, encoding='utf-8') as f:
        programmisisu = f.read()
        süntaksipuu = ast.parse(programmisisu)
        for tipp in ast.walk(süntaksipuu):
            if isinstance(tipp, ast.FunctionDef) and tipp.name == funktsiooninimi:
                for alamtipp in ast.walk(tipp):
                    if isinstance(alamtipp, ast.Call):
                        if 'func' in alamtipp._fields and 'id' in alamtipp.func._fields and alamtipp.func.id == kontrollitavnimi:
                            return True
    return False
     

def progfunkkasutamine(programminimi, funktsiooninimi):    
    if not progpöördub(programminimi, funktsiooninimi):
        teade = "Programmis puudub pöördumine funktsiooni '" + funktsiooninimi +"' poole.\n"
        teade += "Vastus tuleks arvutada programmis defineeritud funktsiooni '" + funktsiooninimi + "' abil."
        lõpeta(teade)

def kasutabmoodulit(programminimi, moodulinimi):
    with open(programminimi, encoding='utf-8') as f:
        programmisisu = f.read()
        if re.search(r'import\s+' + moodulinimi, programmisisu) or re.search(r'from\s+' + moodulinimi + r'\s+import', programmisisu):
            return True
        else:
            return False
        
def moodulikasutamine(programminimi, moodulinimi):    
    if not kasutabmoodulit(programminimi, moodulinimi):
        teade = "Programm ei kasuta moodulit '" + moodulinimi +"'.\n"
        lõpeta(teade)
        
def tühiprintteade(programminimi, funktsiooninimi):        
    teade = "Funktsioon '" + str(funktsiooninimi) + "' ei tagastanud väärtust."
    if funkpöördub(programminimi, funktsiooninimi, "print"):
        teade += "\nVäärtus tuleks funktsioonis tagastada, mitte väljastada."        
    lõpeta(teade)

def funktüüp(funktsiooninimi, tegelik_tulemus, õige_tüüp):
    if type(tegelik_tulemus) != õige_tüüp:
        teade = "Funktsioon '" + str(funktsiooninimi) + "' tagastas väärtuse, mis ei ole "
        if õige_tüüp == str:
            teade += "sõne"
        elif õige_tüüp == list:
            teade += "järjend"
        elif õige_tüüp == int:
            teade += "täisarv"
        else:
            teade += "õiget tüüpi"
        teade += ".\n"
        teade += "Tagastatud väärtus oli "
        if type(tegelik_tulemus) == str: teade += "'"
        teade += str(tegelik_tulemus)
        if type(tegelik_tulemus) == str: teade += "'"        
        teade += "."
        lõpeta(teade)
    
