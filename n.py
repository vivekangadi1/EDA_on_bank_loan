from flask import Flask, request
from flask_restful import Resource, Api
from sqlalchemy import create_engine
from json import dumps



app = Flask(__name__)
api = Api(app)

class Employees(Resource):
    def get(self):
       
        return {'employees': "helloworld 1"} # Fetches first column that is Employee ID

class Tracks(Resource):
    def get(self):
        return {'employees': "helloworld 2"} # Fetches first column that is Employee ID

class Employees_Name(Resource):
    def get(self, employee_id):
       return {'employees': "helloworld 3"} # Fetches first column that is Employee ID
        

api.add_resource(Employees, '/employees') # Route_1
api.add_resource(Tracks, '/tracks') # Route_2
api.add_resource(Employees_Name, '/employees/<employee_id>') # Route_3


if __name__ == '__main__':
     app.run(port='5002')
     

	 
https://stackoverflow.com/questions/22172604/convert-image-url-to-base64

https://stackoverflow.com/questions/2323128/convert-string-in-base64-to-image-and-save-on-filesystem-in-python

venv\Scripts\activate.bat
>pip install flask flask-jsonpify flask-sqlalchemy flask-restful
pip freeze
>python c:\Users\vivek\Desktop\n.py flask run