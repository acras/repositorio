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
`svn mkdir http://svn.acras.com.br/#{nome_projeto} -m "Criando projeto #{nome_projeto}"`
`svn mkdir http://svn.acras.com.br/#{nome_projeto}/trunk -m "Criando trunk"`
`svn mkdir http://svn.acras.com.br/#{nome_projeto}/branches -m "Criando branches"`
`svn mkdir http://svn.acras.com.br/#{nome_projeto}/tags -m "Criando tags"`
`svn co http://svn.acras.com.br/#{nome_projeto}/trunk #{nome_projeto}`

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
http://svn.acras.com.br/rails_plugins/trunk/vendor/plugins/redhillonrails_core/
http://svn.acras.com.br/rails_plugins/trunk/vendor/plugins/foreign_key_migrations/
http://svn.acras.com.br/rails_plugins/trunk/vendor/plugins/validates_existence/
http://svn.acras.com.br/rails_plugins/trunk/vendor/plugins/rspec/
http://svn.acras.com.br/rails_plugins/trunk/vendor/plugins/rspec-rails
http://svn.acras.com.br/rails_plugins/trunk/vendor/plugins/ac_core_extensions/
http://svn.acras.com.br/rails_plugins/trunk/vendor/plugins/ac_jasper_report/
http://svn.acras.com.br/rails_plugins/trunk/vendor/plugins/engines
http://svn.acras.com.br/rails_plugins/trunk/vendor/plugins/ac_resources_permission/
http://svn.acras.com.br/rails_plugins/trunk/vendor/plugins/calendar_date_select/
http://svn.acras.com.br/rails_plugins/trunk/vendor/plugins/acts_as_tree/
}
plugins.each do | p |
    `ruby script/plugin install -x #{p}`
end
