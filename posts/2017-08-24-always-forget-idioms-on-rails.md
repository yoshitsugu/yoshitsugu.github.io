---
title: "[Ruby on Rails] 5 idioms I always forget"
tags: Ruby on Rails
---
Ruby on Rails is the great web application framework. I usually use it in the current job. But I always forget some idioms. In this post, I try to write out them.<!--more-->

## 1. Calling helper methods in Model methods

### Idiom
`ApplicationController.helpers.(helper name)`

### Example
```ruby
class Post < ApplicationRecord
  def title_asterisk
    ApplicationController.helpers.with_asterisk(title)
  end
end
```
## 2. Calling routing helper methods in Model methods
### Idiom
`Rails.application.routes.url_helpers.(routing helper name)`

### Example
```ruby
class Post < ApplicationRecord
  def new_or_edit_path
    if persisted?
      Rails.application.routes.url_helpers.new_post_path
    else
      Rails.application.routes.url_helpers.post_path(self)
    end
  end
end
```

## 3. Updating Model's attributes without saving it to Database
### Idiom
`model.assign_attributes(attributes)`

### Example
```ruby
class PostController < ApplicationController
  def update
    post = Post.find(params[:id])
    post.assign_attributes(post_params)
    post.save! if post.valid?
    redirect_to posts_path
  end
end
```

## 4. Turning off updating `updated_at` when saving Models
### Idiom
`ActiveRecord::Base.record_timestamps = false`

### Example
```ruby
class PostImageUpdater
  def execute
    ActiveRecord::Base.record_timestamps = false
    Post.all.find_each do |post|
      post.image = get_updated_image(post)
    end
    ActiveRecord::Base.record_timestamps = true
  end
end
```


## 5. Basic Auth
### Idiom
`authenticate_or_request_with_http_basic(realm = "Application", &login_procedure)`

### Example
```ruby
class WithBasicAuthController < ApplicationController
  before_action :basic_auth

  def basic_auth
    authenticate_or_request_with_http_basic("Application") do |name, password|
      name == ENV['BASIC_AUTH_NAME'] && password == ENV['BASIC_AUTH_PASSWORD']
    end
  end
end
```

## And More...
Maybe there are more idioms I always googling them. If anyone who have a good idea to remember them, please tell me :)
