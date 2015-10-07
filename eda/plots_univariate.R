# Funci?n para obtener gr?ficas de una variable num?rica
# gracias a Carlos, Amanda y Liliana (itam-dm, 2014)
graficaNum <- function(base, x1){
    
    grafica_0a <- ggplot(base, aes(x = base$id, y = base[,x1]))
    grafica_0b <- ggplot(base, aes(x = base[,x1]))
    
    # Box-plot
    grafica_1 <- grafica_0a + 
        geom_boxplot(fill = '#3399CC', colour = 'black', outlier.colour = 'red', outlier.size = 3) +
        ggtitle(paste('Box-plot ', names(base)[x1])) + 
        scale_y_continuous(name = '') + 
        scale_x_continuous(name = '', breaks = NULL) +
        theme(plot.title = element_text(lineheight = .8, face = 'bold'))
    
    # Histograma
    grafica_2 <- grafica_0b + 
        geom_histogram(fill = '#33CC99', colour = 'black') +
        ggtitle(paste('Histograma ', names(base)[x1])) + 
        scale_x_continuous(name = '') +
        theme(plot.title = element_text(lineheight = .8, face = 'bold'))
    
    # Dot-plot
    grafica_3 <- grafica_0b + 
        geom_dotplot(stackdir = 'centerwhole', fill = '#CC99CC') +
        ggtitle(paste('Dot-plot ', names(base)[x1])) + 
        scale_x_continuous(name = '') +
        theme(plot.title = element_text(lineheight = .8, face = 'bold'))
    
    # Violin-plot
    grafica_4 <- grafica_0a + 
        geom_violin(fill = '#FF9966') +
        ggtitle(paste('Violin-plot ', names(base)[x1])) + 
        scale_y_continuous(name = '') + 
        scale_x_continuous(name = '', breaks = NULL) +
        theme(plot.title = element_text(lineheight = .8, face = 'bold'))
    
    # Densidad
    grafica_5 <- grafica_0b + 
        geom_histogram(aes(y = ..density..), fill = '#FFFFCC', colour = 'black') + 
        geom_density(color = 'red') +
        ggtitle(paste('Densidad ', names(base)[x1])) + 
        scale_x_continuous(name = '') +
        theme(plot.title = element_text(lineheight = .8, face = 'bold'))
    
    # QQ-plot
    
    # Variables que nos serviran para la qqline
    yy <- quantile(base[,x1][!is.na(base[,x1])], c(0.25, 0.75))
    xx <- qnorm(c(0.25, 0.75))
    slope <- diff(yy) / diff(xx)
    int <- yy[1L] - slope * xx[1L]
    
    # Generamos la gr?fica qqnorm y qqline
    grafica_6 <- ggplot(base, aes(sample = base[,x1])) +
        ggtitle(paste('QQ-plot ', names(base)[x1])) + 
        stat_qq(shape = 1, size = 4) +
        geom_abline(slope = slope, intercept = int, colour = 'red', size = 1) +
        theme(plot.title = element_text(lineheight = .8, face = 'bold'))
    
    # Presentamos las gr?ficas en la misma pantalla
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 3)))
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    print(grafica_1, vp = vplayout(1, 1))
    print(grafica_2, vp = vplayout(1, 2))
    print(grafica_3, vp = vplayout(1, 3))
    print(grafica_4, vp = vplayout(2, 1))
    print(grafica_5, vp = vplayout(2, 2))
    print(grafica_6, vp = vplayout(2, 3))
}