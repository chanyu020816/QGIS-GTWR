from qgis.PyQt.QtCore import QCoreApplication, QVariant
from qgis.PyQt.QtCore import QCoreApplication
from qgis.core import (
    QgsProcessing,
    QgsFeatureSink,
    QgsProcessingException,
    QgsProcessingAlgorithm,
    QgsProcessingParameterFeatureSink,
    QgsProcessingParameterField,
    QgsProcessingParameterBoolean,
    QgsFields,
    QgsField,
    QgsFeature,
    QgsProcessingParameterFile,
    QgsWkbTypes,
    QgsPointXY, 
    QgsGeometry
)
from qgis import processing                 

import os
import datetime
import numpy as np
import geopandas as gpd

class FlowEstimateAlgorithm(QgsProcessingAlgorithm):
    ID = 'ID'
    INPUT = 'INPUT'
    OUTPUT = 'OUTPUT'
    OUTPUTVCOMP = 'OUTPUTVCOMP'
    OUTPUTVCOMP_CHECKBOX = 'OUTPUTVCOMP_CHECKBOX'
    VARS = 'VARS'
    
    def tr(self, string):
        return QCoreApplication.translate('Processing', string)
    
    def name(self):
        return 'flow_estimate'

    # 在QGIS上顯示的名稱 
    def displayName(self):
        return self.tr('Flow Estimate')
    
    # 在QGIS上顯示的群組名稱 
    def group(self):
        return self.tr('Flow Estimate Group')
    
    def groupId(self):
        return 'flow_estimate_group'
    
    def createInstance(self):
        return FlowEstimateAlgorithm()
    
    
    def shortHelpString(self):
        return self.tr("Algorithm description")

    # 定義輸入輸出參數
    def initAlgorithm(self, config=None):

        #self.addParameter(
        #    QgsProcessingParameterVectorLayer(
        #        self.INPUT, 
        #        description =  self.tr('Input Layer'),
        #        types = [QgsProcessing.TypeVector],
        #        defaultValue=None,
        #        optional = False
        #    )
        #)
        self.addParameter(
            QgsProcessingParameterFile(
                name = self.INPUT,
                description = 'Open shp file containing geographical coordinates',
                defaultValue=None,
                optional = False,
                fileFilter='SHP Files (*.shp)', 
            )
          )
        
        self.addParameter(
            QgsProcessingParameterField(
                self.ID,
                self.tr('ID Var'),
                allowMultiple = False,
                parentLayerParameterName = self.INPUT
            )
        )
        
        self.addParameter(
            QgsProcessingParameterField(
                self.VARS,
                self.tr('Columns'),
                allowMultiple = True,
                parentLayerParameterName = self.INPUT
            )
        )
        
        self.addParameter(
            QgsProcessingParameterFeatureSink(
                self.OUTPUT,
                self.tr('Output layer')
            )
        )
        
        self.addParameter(
            QgsProcessingParameterBoolean(
                self.OUTPUTVCOMP_CHECKBOX,
                self.tr('Output VComp layer')
            )
        )
        
        self.addParameter(
            QgsProcessingParameterFeatureSink(
                self.OUTPUTVCOMP,
                self.tr('VComp layer')
            )
        )

    # 程序執行部分
    def processAlgorithm(self, parameters, context, feedback):
        results = {}
        # get the input layer as source
        source = self.parameterAsSource(parameters, self.INPUT, context)
        if source is None:
            raise QgsProcessingException(self.invalidSourceError(parameters, self.INPUT))
    
        gdf = gpd.read_file(parameters[self.INPUT])

        # time_vars = layer_attributes[parameters[self.VARS]].values
        feedback.pushInfo(f"TimeStamps: {len(parameters[self.VARS])}")
        feedback.setProgress(5)
        timestamps = len(parameters[self.VARS])
        uid    = parameters[self.ID]

        ifname = os.path.splitext(parameters[self.INPUT])[0]
        #建立參考時間的輸出檔名
        currenttime = datetime.datetime.now().strftime('%Y%m%d%H%M%S')
        ofname = ifname + '_flow_' + currenttime + '.shp'

        gdf_flow = []
        gdf['centroid'] = gdf.centroid
        gdf_flow = gdf[[uid,'centroid']]
        gdf_flow = gpd.GeoDataFrame(gdf_flow, geometry="centroid")

#-->
        gdf_vcomp = []
        gdf['centroid'] = gdf.centroid
        gdf_vcomp = gdf[[uid,'centroid']]
        gdf_vcomp = gpd.GeoDataFrame(gdf_vcomp, geometry="centroid")
#<--

        feedback.pushInfo(f"Loading Shapefile ...")
        
        Xmin = gdf.total_bounds[0] #xmin
        Ymin = gdf.total_bounds[1] #ymin
        Xmax = gdf.total_bounds[2] #xmax
        Ymax = gdf.total_bounds[3] #ymax

        #網格大小
        grid_size_x = int(gdf['geometry'][0].bounds[2] - gdf['geometry'][0].bounds[0]) + 1
        #grid_size_y = int(gdf['geometry'][0].bounds[3] - gdf['geometry'][0].bounds[1]) + 1

        Xi_range = int((Xmax - Xmin)/grid_size_x) + 1
        Yi_range = int((Ymax - Ymin)/grid_size_x) + 1

        feedback.setProgress(10)
        Matrix = [[[0 for y in range(Yi_range)] for x in range(Xi_range)] for t in range(timestamps)]
        Gridid = [[0 for y in range(Yi_range)] for x in range(Xi_range)]

        #3D array
        for index, grid in gdf.iterrows():
            x = round(grid.geometry.centroid.x,3)
            y = round(grid.geometry.centroid.y,3)
            print(grid.iloc[0])
            xi = int((x - Xmin)/grid_size_x)
            yi = int((y - Ymin)/grid_size_x)

            Gridid[xi][yi] = grid[parameters[self.ID]] #grid.YKR_ID

            for j in range(0,timestamps):
                var = parameters[self.VARS][j]
                Matrix[j][xi][yi] = grid[var] 
        feedback.setProgress(15)
        fx = [[[0 for y in range(Yi_range)] for x in range(Xi_range)] for t in range(timestamps)]
        fy = [[[0 for y in range(Yi_range)] for x in range(Xi_range)] for t in range(timestamps)]
        ft = [[[0 for y in range(Yi_range)] for x in range(Xi_range)] for t in range(timestamps)]
        fg = [[[0 for y in range(Yi_range)] for x in range(Xi_range)] for t in range(timestamps)] # gradient value

        progress = 70 / Xi_range
        for xi in range(0,Xi_range):
            for yi in range(0,Yi_range):
                if xi>0 and (xi+1)<Xi_range and yi>0 and (yi+1)<Yi_range:
                    for j in range(1,timestamps-1): # 扣除頭尾
                        vx = 0
                        vy = 0
                        vt = 0
                        vw = 0
                        #sobel
                        vx = 1*(Matrix[j+1][xi+1][yi+1]-Matrix[j+1][xi-1][yi+1])+2*(Matrix[j+1][xi+1][yi+0]-Matrix[j+1][xi-1][yi+0]) \
                            + 1*(Matrix[j+1][xi+1][yi-1]-Matrix[j+1][xi-1][yi-1])+2*(Matrix[j+0][xi+1][yi+1]-Matrix[j+0][xi-1][yi+1]) \
                            + 4*(Matrix[j+0][xi+1][yi+0]-Matrix[j+0][xi-1][yi+0])+2*(Matrix[j+0][xi+1][yi-1]-Matrix[j+0][xi-1][yi-1]) \
                            + 1*(Matrix[j-1][xi+1][yi+1]-Matrix[j-1][xi-1][yi+1])+2*(Matrix[j-1][xi+1][yi+0]-Matrix[j-1][xi-1][yi+0]) \
                            + 1*(Matrix[j-1][xi+1][yi-1]-Matrix[j-1][xi-1][yi-1])

                        vy = 1*(Matrix[j+1][xi+1][yi+1]-Matrix[j+1][xi+1][yi-1])+2*(Matrix[j+1][xi+0][yi+1]-Matrix[j+1][xi+0][yi-1]) \
                            + 1*(Matrix[j+1][xi-1][yi+1]-Matrix[j+1][xi-1][yi-1])+2*(Matrix[j+0][xi+1][yi+1]-Matrix[j+0][xi+1][yi-1]) \
                            + 4*(Matrix[j+0][xi+0][yi+1]-Matrix[j+0][xi+0][yi-1])+2*(Matrix[j+0][xi-1][yi+1]-Matrix[j+0][xi-1][yi-1]) \
                            + 1*(Matrix[j-1][xi+1][yi+1]-Matrix[j-1][xi+1][yi-1])+2*(Matrix[j-1][xi+0][yi+1]-Matrix[j-1][xi-0][yi-1]) \
                            + 1*(Matrix[j-1][xi-1][yi+1]-Matrix[j-1][xi-1][yi-1])

                        vt = 1*(Matrix[j+1][xi+1][yi+1]-Matrix[j-1][xi+1][yi+1])+2*(Matrix[j+1][xi+1][yi+0]-Matrix[j-1][xi+1][yi+0]) \
                            + 1*(Matrix[j+1][xi+1][yi-1]-Matrix[j-1][xi+1][yi-1])+2*(Matrix[j+1][xi+0][yi+1]-Matrix[j-1][xi+0][yi+1]) \
                            + 4*(Matrix[j+1][xi+0][yi+0]-Matrix[j-1][xi+0][yi+0])+2*(Matrix[j+1][xi+0][yi-1]-Matrix[j-1][xi+0][yi-1]) \
                            + 1*(Matrix[j+1][xi-1][yi+1]-Matrix[j-1][xi-1][yi+1])+2*(Matrix[j+1][xi-1][yi+0]-Matrix[j-1][xi-1][yi+0]) \
                            + 1*(Matrix[j+1][xi-1][yi-1]-Matrix[j-1][xi-1][yi-1])

                        gval = np.sqrt(vx**2 + vy**2 + vt**2)

                        if gval > 0:
                            fx[j][xi][yi] = vx / gval  #單位向量
                            fy[j][xi][yi] = vy / gval
                            ft[j][xi][yi] = vt / gval
                            fg[j][xi][yi] = gval
                        else:
                            fx[j][xi][yi] = 0
                            fy[j][xi][yi] = 0
                            ft[j][xi][yi] = 0
                            fg[j][xi][yi] = 0

            feedback.setProgress(15 + progress * xi)

        # 這部分我不確定需不需要加入，原先程式碼有，不過後續我看都沒有調整到這部分內容，在範例輸出也沒有這些Cols，因此我先comment掉
        #for i in range(1,timestamps-1):
        #    timestamp_field = 'vect_' + str(i+1)
        #    gradient_field = 'grad_' + str(i+1)

        #    gdf_flow[timestamp_field] = 0
        #    gdf_flow[gradient_field] = 0

        #step 3

        for index, grid in gdf.iterrows():
            x = round(grid.geometry.centroid.x,3)
            y = round(grid.geometry.centroid.y,3)

            xi = int((x - Xmin)/grid_size_x)
            yi = int((y - Ymin)/grid_size_x)

            if xi>0 and (xi+1)<Xi_range and yi>0 and (yi+1)<Yi_range:
                for j in range(1,timestamps-1):
                    tfield_name = 'T' + str(j+1)
                    gfield_name = 'G' + str(j+1)

#-->
                    vxfield = 'v_' + str(j+1) + '_x'
                    vyfield = 'v_' + str(j+1) + '_y'
                    vzfield = 'v_' + str(j+1) + '_z'
#<--					
                    ax = fx[j][xi][yi]
                    ay = fy[j][xi][yi]
                    at = ft[j][xi][yi]

                    initial_bearing = np.arctan2(ax, ay)
                    initial_bearing = np.degrees(initial_bearing)
                    azi = (initial_bearing + 360) % 360

                    if(at < 0):
                        cazi = (azi + 180) % 360
                    else:
                        cazi= azi

                    gdf_flow.at[index, tfield_name] = cazi
                    gdf_flow.at[index, gfield_name] = fg[j][xi][yi]
#-->
                    gdf_vcomp.at[index, vxfield] = ax
                    gdf_vcomp.at[index, vyfield] = ay
                    gdf_vcomp.at[index, vzfield] = at
#<--
        feedback.setProgress(90)
        # output_folder = parameters[self.OUTPUTFOLDER]
        file_name = ofname.split("/")[-1]
        file_name = file_name.split("\\")[-1]
        # ofname = output_folder + "/" + file_name
        gdf_flow.fillna(0, inplace=True)
        # gdf_flow.to_file(ofname)
        feedback.setProgress(95)
        # gdf_flow = gdf_flow.drop("centroid", axis = 1)
        
        headers=[col for col in gdf_flow.columns]

        headers.remove("centroid")
        fieldlist=QgsFields()
        fieldlist.append(QgsField(headers[0],QVariant.Int))
    
        for name in headers[1:]:
            fieldlist.append(QgsField(name,QVariant.String))
        
        (sink, dest_id) = self.parameterAsSink(
            parameters,
            self.OUTPUT,
            context,
            fieldlist,
            QgsWkbTypes.Point, 
            source.sourceCrs()
        )
        
        if sink is None:
            raise QgsProcessingException(self.invalidSinkError(parameters, self.OUTPUT))
        
        for i in gdf_flow.index.to_list():
            featur=QgsFeature()
            newrow=gdf_flow[headers].iloc[i].tolist()
            featur.setAttributes(newrow)
            point = QgsPointXY(gdf_flow.iloc[i].centroid.x, gdf_flow.iloc[i].centroid.y)
            featur.setGeometry(QgsGeometry.fromPointXY(point))
            sink.addFeature(featur, QgsFeatureSink.FastInsert)
            results[self.OUTPUT] = dest_id

        self.dest_id=dest_id    
        
        if parameters[self.OUTPUTVCOMP_CHECKBOX] == True:
            headers_vcomp=[col for col in gdf_vcomp.columns]

            headers_vcomp.remove("centroid")
            fieldlist_vcomp=QgsFields()
            fieldlist_vcomp.append(QgsField(headers_vcomp[0],QVariant.Int))
        
            for name in headers_vcomp[1:]:
                fieldlist_vcomp.append(QgsField(name,QVariant.String))
            
            (sink_vcomp, dest_vcomp_id) = self.parameterAsSink(
                parameters,
                self.OUTPUTVCOMP,
                context,
                fieldlist_vcomp,
                QgsWkbTypes.Point, 
                source.sourceCrs()
            )
            
            if sink_vcomp is None:
                raise QgsProcessingException(self.invalidSinkError(parameters, self.OUTPUT))
            
            
            for i in gdf_vcomp.index.to_list():
                featur=QgsFeature()
                newrow=gdf_vcomp[headers_vcomp].iloc[i].tolist()
                featur.setAttributes(newrow)
                point = QgsPointXY(gdf_vcomp.iloc[i].centroid.x, gdf_vcomp.iloc[i].centroid.y)
                featur.setGeometry(QgsGeometry.fromPointXY(point))
                sink_vcomp.addFeature(featur, QgsFeatureSink.FastInsert)
                results[self.OUTPUTVCOMP] = dest_vcomp_id

            self.dest_vcomp_id=dest_vcomp_id    
        else: 
            pass
        
        feedback.setProgress(100)
        return results
