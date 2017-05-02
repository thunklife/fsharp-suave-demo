namespace SuaveDemo

    module DB =
        open System.Data
        open System.Dynamic
        open System.Collections.Generic
        open Dapper

        let query<'Result> (query:string) (connection:IDbConnection) =
            connection.Query<'Result>(query)
        
        let parameterizedQuery<'Request, 'Result> (query:string) (param:'Request) (connection:IDbConnection) =
            connection.Query<'Result>(query, param)
        
        let execute<'T> (query:string) (a:'T) (connection : IDbConnection) =
            connection.Execute(query, a)

    module Types =
        open System
        open Chiron
        open Chiron.Operators

        module E = Chiron.Serialization.Json.Encode
        module D = Chiron.Serialization.Json.Decode

        type Employee = { 
            emp_no : int 
            birth_date : DateTime
            first_name : string
            last_name : string
            gender : string
            hire_date : DateTime 
        }
        
        type NewEmployee = { 
            birth_date : DateTime
            first_name : string
            last_name : string
            gender : string
            hire_date : DateTime 
        }

        type EmployeeRequest = { id : int }

        let decodeNewEmployee (jObj:JsonObject): JsonResult<NewEmployee> =
            ((fun b f l g h -> { birth_date = b; first_name = f; last_name = l; gender = g; hire_date = h })
            <!> D.required D.dateTime "birth_date"
            <*> D.required D.string "first_name"
            <*> D.required D.string "last_name"
            <*> D.required D.string "gender"
            <*> D.required D.dateTime "hire_date") jObj

        type NewEmployee with 
            static member FromJson(_:NewEmployee) =
                    let decode jObj = decodeNewEmployee(jObj)
                    D.jsonObject >=> decode

        let encodeEmployee x jObj =
            jObj
            |> E.required E.int "emp_no" x.emp_no
            |> E.required E.string "first_name" x.first_name
            |> E.required E.string "last_name" x.last_name
            |> E.required E.string "gender" x.gender
            |> E.required E.dateTime "birth_date" x.birth_date
            |> E.required E.dateTime "hire_date" x.hire_date

        type Employee with
            static member ToJson (x:Employee) =
                E.buildWith encodeEmployee x


    module Repo =
        open ConnectionString
        open DB
        open Types
        open MySql.Data.MySqlClient
        open System


        let mySqlConnection = new MySqlConnection(connectionString);

        (* This should be either *)
        let private optionalEmployee xs =
            match Seq.isEmpty xs with
            | true -> None
            | false -> Some (Seq.head xs)

        let private getEmployees connection =
            connection
            |> query<Employee> "SELECT * FROM employees" 

        let private getEmployee connection (empNo:int) =
            connection 
            |> parameterizedQuery<EmployeeRequest, Employee> "SELECT * FROM employees WHERE emp_no = @id" {id = empNo} 
            |> optionalEmployee
        
        let private addEmployeeToDb connection (emp:NewEmployee) = 
            connection
            |> execute<NewEmployee> "INSERT INTO employees (birth_date, first_name, last_name, gender, hire_date) VALUES (@birth_date, @first_name, @last_name, @gender, @hire_date)" emp
        
        let addEmployee = addEmployeeToDb mySqlConnection

        let getNEmployees n = getEmployees mySqlConnection |> Seq.take n

        let getSingleEmployee = getEmployee mySqlConnection

    module UseCases =
        open Repo
        open Types

        open Chiron
        open Suave
        open Suave.Filters
        open Suave.Operators
        open Suave.Successful
        open Suave.RequestErrors
        open Suave.Writers

        let getEmployeeFromRequest (r: HttpRequest) =
            let getEmployee rf  : JsonResult<NewEmployee> = 
                rf 
                |> System.Text.Encoding.UTF8.GetString
                |> Inference.Json.deserialize
            r.rawForm |> getEmployee
        
        (* Inserting into the DB gives back an Int would be better to have response types *)
        let handleResponse n =
            match n with 
            | 0 -> "No dice"
            | _ -> n.ToString()

        let getEmployeesHander =
            getNEmployees 20 
            |> Seq.toList 
            |> Inference.Json.serialize 
            |> OK

        let getEmployeeByIdHandler =
            getSingleEmployee 
            >> Option.map Inference.Json.serialize 
            >> Option.get 
            >> OK
        
        let postEmployeeHandler = 
            getEmployeeFromRequest 
            >> JsonResult.map addEmployee 
            >> JsonResult.map handleResponse 
            >> JsonResult.getOrThrow 
            >> OK

        let app = 
            choose [
                GET >=> choose [
                    path "/" >=> OK "Hello World"
                    path "/employees" >=> getEmployeesHander
                    pathScan "/employees/%d" getEmployeeByIdHandler
                ]
                POST  >=> path "/employees" >=> request postEmployeeHandler
                NOT_FOUND "No Handlers Found" 
            ] >=> Suave.Writers.setMimeType "application/json;utf-8"

        let run =
            startWebServer defaultConfig app

    module Main =
        open UseCases
        [<EntryPoint>]
        let main argv = 
            run
            0