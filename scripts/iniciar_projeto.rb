def verifica_params
  if !$*[0]
    puts 'Sintaxe: inicia_projeto <nome_do_projeto>'
    exit
  end
  
end

verifica_params
nome_projeto = $*[0]

puts "Iniciando"
puts "Criando repositorio"
`svn mkdir http://acrassistemas.no-ip.info:8080/svn/#{nome_projeto} -m "Criando projeto #{nome_projeto}"`
`svn co http://acrassistemas.no-ip.info:8080/svn/#{nome_projeto}`

puts "Criando aplicacao rails"
`rails.bat -d mysql #{nome_projeto}`
Dir.chdir(nome_projeto)
File.delete('log/development.log')
File.delete('log/production.log')
File.delete('log/server.log')
File.delete('log/test.log')
File.delete('public/index.html')
`svnaddnew`
`svn propset svn:ignore *.log ./log`

puts "Criando banco de dados"
`"C:/Program Files/MySQL/MySQL Server 5.0/bin/mysql" -u root -e "create database #{nome_projeto}_development"`
`"C:/Program Files/MySQL/MySQL Server 5.0/bin/mysql" -u root -e "create database #{nome_projeto}_test"`
`rake.bat db:migrate`
`svn propset svn:ignore schema.rb ./db`

puts "Commitando projeto iniciado"
`svn ci -m "Iniciando projeto #{nome_projeto}"`

puts "Instalando plugins"
plugins = %w{
http://acrassistemas.no-ip.info:8080/svn/rails_plugins/trunk/vendor/plugins/redhillonrails_core/
http://acrassistemas.no-ip.info:8080/svn/rails_plugins/trunk/vendor/plugins/foreign_key_migrations/
http://acrassistemas.no-ip.info:8080/svn/rails_plugins/trunk/vendor/plugins/validates_existence/
http://acrassistemas.no-ip.info:8080/svn/rails_plugins/trunk/vendor/plugins/rspec/
http://acrassistemas.no-ip.info:8080/svn/rails_plugins/trunk/vendor/plugins/rspec-rails
http://acrassistemas.no-ip.info:8080/svn/rails_plugins/trunk/vendor/plugins/ac_core_extensions/
http://acrassistemas.no-ip.info:8080/svn/rails_plugins/trunk/vendor/plugins/ac_jasper_report/
http://acrassistemas.no-ip.info:8080/svn/rails_plugins/trunk/vendor/plugins/engines
http://acrassistemas.no-ip.info:8080/svn/rails_plugins/trunk/vendor/plugins/ac_resources_permission/
http://acrassistemas.no-ip.info:8080/svn/rails_plugins/trunk/vendor/plugins/calendar_date_select/
http://acrassistemas.no-ip.info:8080/svn/rails_plugins/trunk/vendor/plugins/acts_as_tree/
}
plugins.each do | p |
    `ruby script/plugin install -x #{p}`
end
