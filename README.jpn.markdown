# Caveman2 - 軽量なWebアプリケーションフレームワーク

[![Build Status](https://travis-ci.org/fukamachi/caveman.svg?branch=master)](https://travis-ci.org/fukamachi/caveman)

## 利用方法

```common-lisp
(defparameter *web* (make-instance '<app>))

@route GET "/"
(defun index ()
  (render #P"index.tmpl"))

@route GET "/hello"
(defun say-hello (&key (|name| "Guest"))
  (format nil "Hello, ~A" |name|))
```

## Caveman2とは？

### Caveman1との相違点

Caveman2は、ゼロから書き直されました。

重要な点は、次の通りです:

* [ningle](http://8arrow.org/ningle/)を元にしていること
* データベースとの連携ができること
* 開発環境の切り替えができること([Envy](https://github.com/fukamachi/envy)の利用)
* 新たなルーティングマクロ

### ゼロから書き直した理由

「ningleとCaveman、どちらを使うべきですか？」「違いは何ですか？」と聞かれることがよくありました。

両者の役割が似ていることが原因だったと思います。

どちらも**小さな**フレームワークであり、データベースをサポートしています。

Caveman2は、**小さな**Webフレームワークではありません。[CL-DBI](https://github.com/fukamachi/cl-dbi)をサポートし、デフォルトでデータベース接続を管理します。

## デザインの目標

Cavemanは、Webアプリケーションの開発で共通する部品を集めたコレクションです。

Cavemanは、3つのルールのもと、開発されました。

* 拡張できること
* 実用的であること
* 何も強要しないこと

## はじめに

あなたがここにきたということは、Caveman(洞窟男)のように暮らすことに、興味があるのでしょうか？

洞窟にはディズニーランドはありませんが、始めるには良い場所です。

さて、洞窟に入りましょう。

### インストール

現在、Caveman2は[Quicklisp](https://www.quicklisp.org/beta/)で入手できます。

```common-lisp
(ql:quickload :caveman2)
```

### プロジェクトのテンプレートを生成する

```common-lisp
(caveman2:make-project #P"/path/to/myapp/"
                       :author "<Your full name>")
;-> writing /path/to/myapp/.gitignore
;   writing /path/to/myapp/README.markdown
;   writing /path/to/myapp/app.lisp
;   writing /path/to/myapp/db/schema.sql
;   writing /path/to/myapp/shlyfile.lisp
;   writing /path/to/myapp/myapp-test.asd
;   writing /path/to/myapp/myapp.asd
;   writing /path/to/myapp/src/config.lisp
;   writing /path/to/myapp/src/db.lisp
;   writing /path/to/myapp/src/main.lisp
;   writing /path/to/myapp/src/view.lisp
;   writing /path/to/myapp/src/web.lisp
;   writing /path/to/myapp/static/css/main.css
;   writing /path/to/myapp/t/myapp.lisp
;   writing /path/to/myapp/templates/_errors/404.html
;   writing /path/to/myapp/templates/index.tmpl
;   writing /path/to/myapp/templates/layout/default.tmpl
```

### ルーティング

Caveman2は、ルーティングを定義するために、２通りの方法を提供しています。

`@route`か`defroute`、どちらを使うかは、あなた次第です。

`@route`は、アノテーションのマクロであり、[cl-annot](https://github.com/arielnetworks/cl-annot)を用いています。

「メソッド」 「URL文字列」 「関数」 を受け取ります。

```common-lisp
@route GET "/"
(defun index ()
  ...)

;; 無名のroute
@route GET "/welcome"
(lambda (&key (|name| "Guest"))
  (format nil "Welcome, ~A" |name|))
```

`@route`は、引数のリスト以外は、Caveman1の`@url`と似ています。

必要がない場合は、引数を指定する必要はありません。

`defroute`は単なるマクロであり、`@route`と同じ機能を提供します。

```common-lisp
(defroute index "/" ()
  ...)

;; 無名のroute
(defroute "/welcome" (&key (|name| "Guest"))
  (format nil "Welcome, ~A" |name|))
```

Cavemanは、ningleを元に作られているので、[Sinatra](http://www.sinatrarb.com/)のようなルーティングも使えます。

```common-lisp
;; GET request (default)
@route GET "/" (lambda () ...)
(defroute ("/" :method :GET) () ...)

;; POST request
@route POST "/" (lambda () ...)
(defroute ("/" :method :POST) () ...)

;; PUT request
@route PUT "/" (lambda () ...)
(defroute ("/" :method :PUT) () ...)

;; DELETE request
@route DELETE "/" (lambda () ...)
(defroute ("/" :method :DELETE) () ...)

;; OPTIONS request
@route OPTIONS "/" (lambda () ...)
(defroute ("/" :method :OPTIONS) () ...)

;; For all methods
@route ANY "/" (lambda () ...)
(defroute ("/" :method :ANY) () ...)
```
ルートパターンには、値を引数に入れるために、**キーワード**を含むことができます。

```common-lisp
(defroute "/hello/:name" (&key name)
  (format nil "Hello, ~A" name))
```

上のコントローラーでは、 "/hello/Eitaro"にアクセスがきたとき`name`は"Eitaro"になり、"/hello/Tomohiro"にアクセスがきたとき`name`は"Tomohiro"になります。

`(&key name)`はCommon Lispのラムダリストとほぼ同じですが、他のkeyを含むこともできます。

```common-lisp
(defroute "/hello/:name" (&rest params &key name)
  ;; ...
  )
```
ルートパターンには、*ワイルドカード*引数を含めることもできます。`splat`でアクセス可能です。

```common-lisp
(defroute "/say/*/to/*" (&key splat)
  ; /say/hello/to/world にマッチします
  splat ;=> ("hello" "world")
  ))

(defroute "/download/*.*" (&key splat)
  ; /download/path/to/file.xml にマッチします
  splat ;=> ("path/to/file" "xml")
  ))
```
URLのルールに正規表現を使うときには、`:regexp t`を明記してください。

```common-lisp
(defroute ("/hello/([\\w]+)" :regexp t) (&key captures)
  (format nil "Hello, ~A!" (first captures)))
```
通常、ルート(routes)は、定義された順番通りにマッチします。

はじめにマッチしたルートが呼びだされ、残りは無視されます。

`next-route`を使うことで、次にマッチするルートに処理を渡すことができます。

```common-lisp
(defroute "/guess/:who" (&key who)
  (if (string= who "Eitaro")
      "You got me!"
      (next-route)))

(defroute "/guess/*" ()
  "You missed!")
```

`defroute`の結果として、次のフォーマットを返すことができます。

* 文字列
* パス名
* Clackのレスポンスのリスト(Status、Headers、Bodyを含みます)

### クエリー問い合わせ/POSTパラメータ

角括弧`"[" & "]"`を含むパラメータのキーは、クエリー問い合わせとしてパースされます。

ルータ(routers)で`_parsed`としてパースされたパラメータにアクセスすることができます。

```html
<form action="/edit">
  <input type="name" name="person[name]" />
  <input type="name" name="person[email]" />
  <input type="name" name="person[birth][year]" />
  <input type="name" name="person[birth][month]" />
  <input type="name" name="person[birth][day]" />
</form>
```

```common-lisp
(defroute "/edit" (&key _parsed)
  (format nil "~S" 
　　　　　　　　(cdr (assoc "person" _parsed :test #'string=))))

;=> "((\"name\" . \"Eitaro\") (\"email\" . \"e.arrows@gmail.com\") (\"birth\" . ((\"year\" . 2000) (\"month\" . 1) (\"day\" . 1))))"
;=> "((\"name\" . \"Eitaro\") (\"email\" . \"e.arrows@gmail.com\") (\"birth\" . ((\"year\" . 2000) (\"month\" . 1) (\"day\" . 1))))"
```

空白のキーは、多値を含むことを表しています。

```html
<form action="/add">
  <input type="text" name="items[][name]" />
  <input type="text" name="items[][price]" />

  <input type="text" name="items[][name]" />
  <input type="text" name="items[][price]" />

  <input type="submit" value="Add" />
</form>
```

```common-lisp
(defroute "/add" (&key _parsed)
  (format nil "~S" (assoc "items" _parsed :test #'string=)))
;=> "(((\"name\" . \"WiiU\") (\"price\" . \"30000\")) ((\"name\" . \"PS4\") (\"price\" . \"69000\")))"
```

### テンプレート

Cavemanは、デフォルトのテンプレートエンジンとして、[Djula](http://mmontone.github.io/djula/)を採用しています。

```html
{% extends "layouts/default.html" %}
{% block title %}Users | MyApp{% endblock %}
{% block content %}
<div id="main">
  <ul>
  {% for user in users %}
    <li><a href="{{ user.url }}">{{ user.name }}</a></li>
  {% endfor %}
  </ul>
</div>
{% endblock %}
```

```common-lisp
(import 'myapp.view:render)

(render #P"users.html"
        '(:users ((:url "/id/1"
                   :name "nitro_idiot")
                  (:url "/id/2"
                   :name "meymao"))
          :has-next-page T))
```
[Djula](http://mmontone.github.io/djula/)を用いて、データベースから何か取得したり、関数を実行するには、どうしたらいいでしょうか。

レンダー(render)に、実行された引数を渡すときには、明示的に`list関数`を使う必要があります。

```common-lisp
(import 'myapp.view:render)

(render #P"users.html"
        (list :users (get-users-from-db)))
```

### JSON API

次は、JSON APIの使用例です。

```common-lisp
(defroute "/user.json" (&key |id|)
  (let ((person (find-person-from-db |id|)))

    ;; person => (:|name| "Eitaro Fukamachi" :|email| "e.arrows@gmail.com")

    (render-json person)))

;=> {"name":"Eitaro Fukamachi","email":"e.arrows@gmail.com"}
```

`render-json`は、スケルトンプロジェクトの一部です。 コードは、"src/view.lisp"にあります。


### 静的なファイル

"static/"フォルダーにある画像、css、js、favicon.ico、robot.txtは、デフォルトでサーブされます。

```
/images/logo.png => {PROJECT_ROOT}/static/images/logo.png
/css/main.css    => {PROJECT_ROOT}/static/css/main.css
/js/app/index.js => {PROJECT_ROOT}/static/js/app/index.js
/robot.txt       => {PROJECT_ROOT}/static/robot.txt
/favicon.ico     => {PROJECT_ROOT}/static/favicon.ico
```
"PROJECT_ROOT/app.lisp"を書き直すことで、このルールを変更することができます。

詳細は、[Clack.Middleware.Static](http://quickdocs.org/clack/api#package-CLACK.MIDDLEWARE.STATIC)を参照してください。

### 環境設定

Cavemanは、環境設定を切り替えるために、[Envy](https://github.com/fukamachi/envy)を採用しています。

[Envy](https://github.com/fukamachi/envy)を使うと、複数の環境を定義して、環境設定を環境変数で切り替えることができます。

次は、典型的な利用方法です。

```common-lisp
(defpackage :myapp.config
  (:use :cl
        :envy))
(in-package :myapp.config)

(setf (config-env-var) "APP_ENV")

(defconfig :common
  `(:application-root ,(asdf:component-pathname (asdf:find-system :myapp))))

(defconfig |development|
  '(:debug T
    :databases
    ((:maindb :sqlite3 :database-name ,(merge-pathnames #P"test.db"
                                                        *application-root*)))))

(defconfig |production|
  '(:databases
    ((:maindb :mysql :database-name "myapp" :username "whoami" :password "1234")
     (:workerdb :mysql :database-name "jobs" :username "whoami" :password "1234"))))

(defconfig |staging|
  `(:debug T
    ,@|production|))
```
全ての環境設定は、plistです。`APP_ENV`を設定することで、どの開発環境を使うかを選ぶことができます。

現在の環境設定から値を得るには、`myapp.config:config`に入手したいキーと一緒に呼び出します。

```common-lisp
(import 'myapp.config:config)

(setf (osicat:environment-variable "APP_ENV") "development")
(config :debug)
;=> T
```

### データベース

`:databases`を環境設定に加えると、Cavemanはデータベースのサポートを有効化します。

`:databases`は、データベースの設定を含んだalistです。

```common-lisp
(defconfig |production|
  '(:databases
    ((:maindb :mysql :database-name "myapp" :username "whoami" :password "1234")
     (:workerdb :mysql :database-name "jobs" :username "whoami" :password "1234"))))
```

`myapp.db`にある`db`は、上で設定された各々のデータベースに接続するための関数です。

次のように使えます。

```common-lisp
(use-package '(:myapp.db :sxql :datafly))

(defun search-adults ()
  (with-connection (db)
    (retrieve-all
      (select :*
        (from :person)
        (where (:>= :age 20))))))
```

接続は、Lispセッションの間は有効であり、それぞれのHTTPリクエストで再利用されます。

`retrieve-all`とクエリ言語は、[datafly](https://github.com/fukamachi/datafly)と[SxQL](https://github.com/fukamachi/sxql)からきています。

詳細は、それぞれのドキュメントをご参照ください。

### HTTPヘッダー/HTTPステータスを設定する

HTTPリクエストの間、特別な変数を利用できます。

`*request*`と`*response*`は、リクエストとレスポンスを表します。

[Clack](http://clacklisp.org/)を使い慣れている場合は、[Clack.Request](http://quickdocs.org/clack/api#package-CLACK.REQUEST)と [Clack.Response](http://quickdocs.org/clack/api#package-CLACK.RESPONSE)のサブクラスのインスタンスもあります。

```common-lisp
(use-package :caveman2)

;; Get a value of Referer header.
(http-referer *request*)

;; Set Content-Type header.
(setf (getf (response-headers *response* :content-type) "application/json")

;; Set HTTP status.
(setf (status *response*) 304)
```

If you would like to set Content-Type "application/json" for all "*.json" requests, `next-route` will help you.

```common-lisp
(defroute "/*.json" ()
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (next-route))

(defroute "/user.json" () ...)
(defroute "/search.json" () ...)
(defroute ("/new.json" :method :POST) () ...)
```

### Using session

セッションデータとは、ユーザー固有のデータを記憶するためのものです。

`*session*`は、セッションデータを表すハッシュテーブルです。

次の例では、セッションでの`:counter`を増やして、それぞれの訪問者に表示しています。

```common-lisp
(defroute "/counter" ()
  (format nil "You came here ~A times."
          (incf (gethash :counter *session* 0))))
```

Caveman2は、デフォルトで、セッションデータをメモリに保存します。

変更するには、"PROJECT_ROOT/app.lisp"にある`:store`を`:session`に指定します。
To change it, specify `:store` to `:session` in .

次の例では、保存するためにRDBMSを使っています。

```diff
      '(:backtrace
        :output (getf (config) :error-log))
      nil)
- :session
+ (:session
+  :store (make-dbi-store :connector (lambda ()
+                                      (apply #'dbi:connect
+                                             (myapp.db:connection-settings)))))
  (if (productionp)
      nil
      (lambda (app)
```

注: あなたのアプリの`:depends-on`として、`:lack-session-store-dbi`を追加するのを忘れないように気をつけてください。

それはClack/Lackの一部ではありません。

詳しい情報は、`Lack.Session.Store.DBi`のコードをご覧ください。

- [Lack.Session.Store.Dbi](https://github.com/fukamachi/lack/blob/master/src/middleware/session/store/dbi.lisp)

### [HTTPステータスコード](https://ja.wikipedia.org/wiki/HTTP%E3%82%B9%E3%83%86%E3%83%BC%E3%82%BF%E3%82%B9%E3%82%B3%E3%83%BC%E3%83%89)を投げる

```common-lisp
(import 'caveman2:throw-code)

(defroute ("/auth" :method :POST) (&key |name| |password|)
  (unless (authorize |name| |password|)
    (throw-code 403)))
```

### エラーページを特定する

404, 500等のエラーページを特定するには、アプリ内で`on-exception`メソッドを定義してください。

```common-lisp
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app code))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
```

### サーバを起動する

あなたのアプリは、起動と停止のために、`start`と`stop`という名前の関数をもっています。

あなたのアプリが"myapp"という名前だとすると、次のようになります。

```common-lisp
(myapp:start :port 8080)
```

Cavemanは、Clack/Lackを元にしているので、Hunchentoot、mod_lisp、FastCGIのどのサーバを使うかを選択することができます。

```common-lisp
(myapp:start :server :hunchentoot :port 8080)
(myapp:start :server :fcgi :port 8080)
```

ローカル環境ではHunchentoot、本番環境ではFastCGIかWooを使うことをおすすめします。

[clackupコマンド](https://github.com/fukamachi/clack/blob/master/roswell/clackup.ros)を使って、アプリを起動することもできます。
[clackup command](https://github.com/fukamachi/clack/blob/master/roswell/clackup.ros).

    $ ros install clack
    $ which clackup
    /Users/nitro_idiot/.roswell/bin/clackup

    $ APP_ENV=development clackup --server :fcgi --port 8080 app.lisp

### ホットデプロイ

Cavemanにはホットデプロイの機能はありませんが、Perlモジュールの[Server::Starter](http://search.cpan.org/~kazuho/Server-Starter-0.15/lib/Server/Starter.pm)を使うと簡単にできます。

    $ APP_ENV=production start_server --port 8080 -- clackup --server :fcgi app.lisp

注: `Server::Starter`は、サーバが特定のFDにバインドできる必要があるので、`start_server`コマンドで動作するのは、`:fcgi`と`:woo`だけです。

サーバを再起動するには、HUPシグナル(`kill -HUP <pid>`)を、`start_server`プロセスに送ってください。

### エラーログ

Cavemanは、エラーのバックトレースを、`:error-log`で特定したファイルに書き出します。

```common-lisp
(defconfig |default|
  `(:error-log #P"/var/log/apps/myapp_error.log"
    :databases
    ((:maindb :sqlite3 :database-name ,(merge-pathnames #P"myapp.db"
                                                        *application-root*)))))
```

## 他のテンプレートエンジンを使う

### CL-WHO

```common-lisp
(import 'cl-who:with-html-output-to-string)

(defroute "/" ()
  (with-html-output-to-string (output nil :prologue t)
    (:html
      (:head (:title "Welcome to Caveman!"))
      (:body "Blah blah blah."))))
;=> "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
;   <html><head><title>Welcome to Caveman!</title></head><body>Blah blah blah.</body></html>"
```

* [CL-WHO Website](http://weitz.de/cl-who/)

### CL-Markup

```common-lisp
(import 'cl-markup:xhtml)

(defroute "/" ()
  (xhtml
    (:head (:title "Welcome to Caveman!"))
    (:body "Blah blah blah.")))
;=> "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\"><html><head><title>Welcome to Caveman!</title></head><body>Blah blah blah.</body></html>"
```

* [CL-Markup repository](https://github.com/arielnetworks/cl-markup)

### cl-closure-template

```html
{namespace myapp.view}

{template renderIndex}
<!DOCTYPE html>
<html>
<head>
  <title>"Welcome to Caveman!</title>
</head>
<body>
  Blah blah blah.
</body>
</html>
{/template}
```

```common-lisp
(import 'myapp.config:*template-directory*)

(closure-template:compile-cl-templates (merge-pathnames #P"index.tmpl"
                                                        *template-directory*))

(defroute "/" ()
  (myapp.view:render-index))
```

* [cl-closure-template](http://quickdocs.org/cl-closure-template/)
* [Closure Templates Documentation](https://developers.google.com/closure/templates/docs/overview)

<!-- Commenting out because these are old.

## 他のデータベース関連ライブラリをつかう

### CLSQL

ClackアプリでCLSQLを使うためには、`Lack.Middleware.Clsql`を使ってください。

Cavemanでは、"PROJECT_ROOT/app.lisp"の`builder`にミドルウェアを追加してください。

```common-lisp
(ql:quickload :clack-middleware-clsql)
(import 'clack.middleware.clsql:<clack-middleware-clsql>)

(builder
 (<clack-middleware-clsql>
  :database-type :mysql
  :connection-spec '("localhost" "db" "fukamachi" "password"))
 *web*)
```

* [Clack.Middleware.Clsql](http://quickdocs.org/clack/api#system-clack-middleware-clsql)
* [CLSQL: Common Lisp SQL Interface](http://clsql.b9.com/)

### Postmodern

ClackアプリでPostmodernを使うためには、`Clack.Middleware.Postmodern`を使ってください。

Cavemanでは、"PROJECT_ROOT/app.lisp"の`builder`にミドルウェアを追加してください。


```common-lisp
(ql:quickload :clack-middleware-postmodern)
(import 'clack.middleware.postmodern:<clack-middleware-postmodern>)

(builder
 (<clack-middleware-postmodern>
  :database "database-name"
  :user "database-user"
  :password "database-password"
  :host "remote-address")
 *web*)
```

* [Clack.Middleware.Postmodern](http://quickdocs.org/clack/api#system-clack-middleware-postmodern)
* [Postmodern](http://marijnhaverbeke.nl/postmodern/)

-->

## 参考

* [Clack](http://clacklisp.org/) - Web application environment.
* [Lack](https://github.com/fukamachi/lack) - The core of Clack.
* [ningle](http://8arrow.org/ningle/) - Super micro web application framework Caveman bases on.
* [Djula](http://mmontone.github.io/djula/) - HTML Templating engine.
* [CL-DBI](http://8arrow.org/cl-dbi/) - Database independent interface library.
* [SxQL](http://8arrow.org/sxql/) - SQL builder library.
* [Envy](https://github.com/fukamachi/envy) - Configuration switcher.
* [Roswell](https://github.com/snmsts/roswell) - Common Lisp implementation manager.

## 作者

* Eitaro Fukamachi (e.arrows@gmail.com)

# ライセンス

Licensed under the LLGPL License.
