"""Planillas para que dos evaluadores juzguen los candidatos de las consultas nuevas (observación O5).

  crear <pool_consultas_nuevas.csv> <dir_salida> [fraccion_comun=0.15] [semilla=20261028] [fuentes=HB]
      Una parte común (fraccion_comun de los candidatos de cada consulta, al menos 5) la juzgan
      los dos evaluadores, para medir su acuerdo; el resto se reparte en mitades. Escribe
      Evaluador_A.xlsx y Evaluador_B.xlsx (sin las fuentes, en orden aleatorio distinto) y
      clave.csv (id → consulta, título, fuentes y a quién se asignó; NO entregar).
      fuentes: solo se juzgan los candidatos que vienen de esas fuentes del pool (por defecto los 20
      primeros del híbrido y la muestra del baseline, que bastan para compararlos con el mismo corte;
      el recall queda relativo a ese pool).

  gt <Evaluador_A.xlsx> <Evaluador_B.xlsx> <clave.csv> <salida.csv> [regla=ambos]
      Junta los juicios: cada fila propia vale lo que dijo su evaluador; en la parte común
      informa el acuerdo y kappa, y resuelve los desacuerdos con la regla «ambos» (relevante solo
      si los dos dijeron Sí) o «alguno» (basta uno). Escribe el ground truth de las consultas
      nuevas con el mismo formato que ground_truth_evaluacion.csv.
"""
import csv
import os
import random
import sys
from collections import defaultdict

from gt_muestra_humana import escribir_planilla, kappa, leer_juicios


def crear(pool_csv, salida, fraccion=0.15, semilla=20261028, fuentes="HB"):
    os.makedirs(salida, exist_ok=True)
    with open(pool_csv, encoding="utf-8") as f:
        pool = list(csv.DictReader(f, delimiter="|", quoting=csv.QUOTE_NONE))
    pool = [p for p in pool if any(c in p["fuentes"] for c in fuentes)]
    rnd = random.Random(semilla)
    por_q = defaultdict(list)
    for p in pool:
        por_q[p["id_consulta"]].append(p)
    filas = []
    for q in sorted(por_q):
        cands = por_q[q][:]
        rnd.shuffle(cands)
        k = max(5, round(fraccion * len(cands)))
        resto = cands[k:]
        for c in cands[:k]:
            filas.append((c, "AB"))
        for i, c in enumerate(resto):
            filas.append((c, "A" if i % 2 == 0 else "B"))
    rnd.shuffle(filas)
    reg = []
    for i, (c, asig) in enumerate(filas, 1):
        reg.append({"id": i, "id_consulta": c["id_consulta"], "query": c["query"], "criterio": c["criterio"],
                    "titulo": c["titulo"], "ano": c["ano"], "palabras_clave": c["palabras_clave"],
                    "resumen": c["resumen"], "fuentes": c["fuentes"], "asignado": asig})
    with open(f"{salida}/clave.csv", "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, delimiter="|")
        w.writerow(["id", "id_consulta", "query", "titulo", "fuentes", "asignado"])
        for r in reg:
            w.writerow([r["id"], r["id_consulta"], r["query"], r["titulo"], r["fuentes"], r["asignado"]])
    a = [r for r in reg if "A" in r["asignado"]]
    b = [r for r in reg if "B" in r["asignado"]]
    random.Random(semilla + 1).shuffle(b)
    escribir_planilla(f"{salida}/Evaluador_A.xlsx", a, "A")
    escribir_planilla(f"{salida}/Evaluador_B.xlsx", b, "B")
    comun = sum(r["asignado"] == "AB" for r in reg)
    print(f"Candidatos: {len(reg)} | comunes: {comun} | evaluador A: {len(a)} | evaluador B: {len(b)}")


def gt(ruta_a, ruta_b, ruta_clave, salida, regla="ambos"):
    with open(ruta_clave, encoding="utf-8") as f:
        clave = {int(r["id"]): r for r in csv.DictReader(f, delimiter="|", quoting=csv.QUOTE_NONE)}
    ja, jb = leer_juicios(ruta_a), leer_juicios(ruta_b)
    for nom, j, letra in (("A", ja, "A"), ("B", jb, "B")):
        esperadas = {i for i, c in clave.items() if letra in c["asignado"]}
        if set(j) != esperadas:
            raise ValueError(f"Evaluador {nom}: las filas no coinciden con la clave")
    comunes = sorted(set(ja) & set(jb))
    a, b = [ja[i] for i in comunes], [jb[i] for i in comunes]
    acuerdo = sum(x == y for x, y in zip(a, b))
    print(f"Parte común: {len(comunes)} filas | acuerdo {acuerdo} ({acuerdo / len(comunes):.1%}) | "
          f"kappa {kappa(a, b):.2f} | Sí A {sum(a)} | Sí B {sum(b)}")
    relevante = {}
    for i, c in clave.items():
        if c["asignado"] == "AB":
            relevante[i] = (ja[i] and jb[i]) if regla == "ambos" else (ja[i] or jb[i])
        else:
            relevante[i] = ja[i] if c["asignado"] == "A" else jb[i]
    por_q = defaultdict(list)
    for i in sorted(clave):
        if relevante[i]:
            por_q[clave[i]["query"]].append(clave[i]["titulo"])
    consultas = sorted({(c["id_consulta"], c["query"]) for c in clave.values()})
    with open(salida, "w", encoding="utf-8", newline="") as f:
        f.write("query|terminos|n_relevantes|relevantes_titulos\n")
        for idq, q in consultas:
            f.write(f"{q}|{idq}|{len(por_q[q])}|{';;'.join(por_q[q])}\n")
            print(f"  {idq} {q}: {len(por_q[q])} relevantes")
    print(f"Regla en la parte común: {regla} | relevantes: {sum(relevante.values())} de {len(clave)} | {salida}")


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    if sys.argv[1] == "crear":
        crear(sys.argv[2], sys.argv[3], *[float(x) for x in sys.argv[4:5]], *[int(x) for x in sys.argv[5:6]],
              *sys.argv[6:7])
    elif sys.argv[1] == "gt":
        gt(*sys.argv[2:7])
