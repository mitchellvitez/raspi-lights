from flask import Flask, render_template
import os
import subprocess
import signal

app = Flask(__name__)

# procedure_name: description
# procedure_name must match the python filename
procedures = {
    'conway': "Conway's Game of Life",
    'splash': "Raindrops",
    'bubble_sort': "Sort by Color"
}

proc = None

@app.route("/")
def index():
    global procedures
    return render_template('index.html', procedures=procedures)

@app.route('/run/<procedure>')
def run(procedure):
    global proc

    if proc:
        pid = proc.pid
        os.kill(pid, signal.SIGINT)

    proc = subprocess.Popen([f'python3 examples/{procedure}.py'], shell=True)

    print (f"running procedure {procedure}")
    return (f"running procedure {procedure}")

app.run(host='0.0.0.0')
